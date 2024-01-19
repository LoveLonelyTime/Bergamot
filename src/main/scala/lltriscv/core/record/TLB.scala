package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.bus.SMAReaderIO
import lltriscv.utils.CoreUtils
import lltriscv.core.execute.MemoryAccessLength

import lltriscv.utils.ChiselUtils._
import lltriscv.cache.FlushCacheIO

/*
 * TLB (Translation Lookaside Buffer)
 *
 * TLB performs virtual address translation and cache PTE to accelerate the entire process.
 * Currently, supports the Sv32 specification.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** DataTLB
  *
  * TLB for accessing to data (load/store)
  *
  * @param depth
  *   TLB depth
  */
class DataTLB(depth: Int) extends Module {
  val io = IO(new Bundle {
    // Request interface
    val request = Flipped(new TLBRequestIO())
    // SMA interface
    val sma = new SMAReaderIO()
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // satp
    val satp = Input(DataType.operation)
    // mstatus
    val mstatus = Input(DataType.operation)

    // Flush request interface
    val flush = Flipped(new FlushCacheIO())
  })

  // TLB
  private val table = RegInit(Vec(depth, new TLBEntry()).zero)

  private val statusReg = RegInit(Status.idle)
  private object Status extends ChiselEnum {
    /*
     * idle: Idle
     * lookup: Fully associative search
     * vpn1: PTE walk vpn1
     * vpn0: PTE walk vpn0
     * flush: Flush TLB
     */
    val idle, lookup, vpn1, vpn0, flush = Value
  }

  private val incrVictim = WireInit(false.B)
  private val (victimPtr, nextVictimPtr) = CoreUtils.pointer(depth, incrVictim)

  private def alloc(pte: UInt, mPage: Bool, global: Bool) = {
    incrVictim := true.B
    val victim = table(victimPtr)
    victim.vpn := io.request.vaddress(31, 12)
    victim.ppn := pte(31, 10)
    victim.da := pte(7, 6)
    victim.g := pte(5) || global
    victim.uxwr := pte(4, 1)
    victim.v := pte(0)
    victim.mPage := mPage
    victim.valid := true.B
  }

  // Empty
  private val validValues = VecInit.fill(depth)(false.B)
  for (i <- 0 until depth) validValues(i) := table(i).valid
  io.flush.empty := !validValues.reduceTree(_ || _)

  private val pagePrivilege = Mux(
    io.privilege === PrivilegeType.M,
    Mux(
      io.mstatus(17), // MPRV
      PrivilegeType.mcode(io.mstatus(12, 11)), // MPP
      PrivilegeType.M
    ),
    io.privilege
  )

  io.request.ready := false.B
  io.request.paddress := 0.U
  io.request.error := TLBErrorCode.success
  // Sample
  when(statusReg === Status.idle) {
    when(io.flush.req) { // Flush
      for (i <- 0 until depth) {
        table(i).valid := false.B
      }
    }.elsewhen(io.request.valid) {
      // MPRV check
      when(pagePrivilege === PrivilegeType.M || !io.satp(31)) { // Page memory disabled
        io.request.paddress := io.request.vaddress
        io.request.error := TLBErrorCode.success
        io.request.ready := true.B
      }.otherwise { // Page memory enabled
        statusReg := Status.lookup
      }
    }
  }

  // Lookup
  when(statusReg === Status.lookup) {
    // Fully-associative
    val grants = VecInit.fill(depth)(false.B)
    for (i <- 0 until depth) {
      val matchAddress = Mux(
        table(i).mPage,
        io.request.vaddress(31, 22) === table(i).vpn(19, 10), // 4MiB page
        io.request.vaddress(31, 12) === table(i).vpn // 4KiB page
      )

      val matchASID = table(i).g || table(i).asid === io.satp(30, 22)
      grants(i) := table(i).valid && matchAddress && matchASID
    }

    when(grants.reduceTree(_ || _)) { // Hit
      for (i <- 0 until depth) {
        when(grants(i)) {
          when(table(i).v && pteDACheck(table(i).da) && ptePrivilegeCheck(table(i).uxwr)) {
            // 34 -> 32
            io.request.paddress := Mux(
              table(i).mPage,
              table(i).ppn(21, 10) ## io.request.vaddress(21, 0), // 4MiB page
              table(i).ppn ## io.request.vaddress(11, 0) // 4KiB page
            )
          }.otherwise { // Fault
            io.request.error := TLBErrorCode.pageFault
          }
        }
      }

      io.request.ready := true.B
      statusReg := Status.idle
    }.otherwise { // Miss
      statusReg := Status.vpn1
    }
  }

  private def pteDACheck(da: UInt) = {
    da(0) && // A is set
    (!io.request.write || da(1)) // Write and D is set or just read
  }

  private def ptePrivilegeCheck(uxwr: UInt) = {
    val r = Mux(io.mstatus(19), uxwr(0) || uxwr(2), uxwr(0)) // MXR
    val w = uxwr(1)
    val x = uxwr(2)
    val u = uxwr(3)

    val xwrResult = Mux(io.request.write, r && w, r)
    val suResult = WireInit(false.B)

    // Check S/U
    switch(pagePrivilege) {
      is(PrivilegeType.S) {
        suResult := Mux(io.mstatus(18), true.B, !u) // SUM
      }
      is(PrivilegeType.U) {
        suResult := u
      }
    }

    xwrResult && suResult
  }

  io.sma.valid := false.B
  io.sma.readType := MemoryAccessLength.byte
  io.sma.address := 0.U

  // TLB walk: vpn1
  private val vpn1Reg = RegInit(DataType.operation.zeroAsUInt)
  when(statusReg === Status.vpn1) {
    // PTE-1 entry address (34 -> 32)
    io.sma.address := io.satp(21, 0) ## io.request.vaddress(31, 22) ## 0.U(2.W)
    io.sma.readType := MemoryAccessLength.word
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) {
        val pte = io.sma.data
        vpn1Reg := pte // Save
        when(pte(4, 2) === "b000".U) { // Next
          when(pte(0)) { // OK
            statusReg := Status.vpn0
          }.otherwise {
            alloc(pte, true.B, false.B)
            statusReg := Status.lookup // Return
          }
        }.otherwise { // Leaf
          alloc(pte, true.B, false.B)
          statusReg := Status.lookup // Return
        }
      }.otherwise { // Memory error
        io.request.error := TLBErrorCode.memoryFault
        io.request.ready := true.B
        statusReg := Status.idle
      }
    }
  }

  // TLB walk: vpn0
  when(statusReg === Status.vpn0) {
    // PTE-2 entry address (34 -> 32)
    io.sma.address := vpn1Reg(31, 10) ## io.request.vaddress(21, 12) ## 0.U(2.W)
    io.sma.readType := MemoryAccessLength.word
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      val pte = io.sma.data
      when(!io.sma.error) { // Leaf
        alloc(pte, false.B, vpn1Reg(5))
        statusReg := Status.lookup // Return
      }.otherwise { // Memory error
        io.request.error := TLBErrorCode.memoryFault
        io.request.ready := true.B
        statusReg := Status.idle
      }
    }
  }
}
