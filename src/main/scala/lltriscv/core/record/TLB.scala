package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.bus.SMAReaderIO
import lltriscv.utils.CoreUtils

// Sv32

// TODO: ASID Support
// TODO: Cache flushing
// TODO: Improving cache performance for failed paths
class DataTLB(depth: Int) extends Module {
  val io = IO(new Bundle {
    val vaddress = Flipped(DecoupledIO(new TLBVAddressEntry()))
    val paddress = DecoupledIO(new TLBPAddressEntry())

    // SMA interface
    val sma = new SMAReaderIO()

    // Current core privilege
    val privilege = Input(PrivilegeType())
    // satp
    val satp = Input(DataType.operation)
    // mstatus
    val mstatus = Input(DataType.operation)
  })

  // TLB
  private val table = Reg(Vec(depth, new TLBEntry()))

  private val vaddressReg = Reg(new TLBVAddressEntry())
  private val paddressReg = Reg(new TLBPAddressEntry())

  private val statusReg = RegInit(Status.idle)

  private val incrVictim = WireInit(false.B)
  private val (victimPtr, nextVictimPtr) = CoreUtils.pointer(depth, incrVictim)

  private def alloc() = {
    incrVictim := true.B
    table(victimPtr)
  }
  object Status extends ChiselEnum {
    val idle, lookup, vpn1, vpn0 = Value
  }

  io.vaddress.ready := statusReg === Status.idle // Idle
  // Finished
  io.paddress.valid := statusReg === Status.idle
  io.paddress.bits := paddressReg

  when(io.vaddress.valid && io.vaddress.ready) { // Sample
    vaddressReg := io.vaddress.bits

    when(io.privilege === PrivilegeType.M || io.satp(31) === 0.U) { // Page memory disabled
      paddressReg.address := io.vaddress.bits
      paddressReg.error := TLBErrorCode.success
    }.otherwise { // Page memory enabled
      statusReg := Status.lookup
    }
  }

  io.sma.valid := false.B
  io.sma.address := 0.U

  when(statusReg === Status.lookup) {
    // Fully-associative
    val grants = VecInit.fill(depth)(false.B)
    for (i <- 0 until depth) {
      val matchAddress = Mux(
        table(i).mPage,
        vaddressReg.address(31, 22) === table(i).vpn(19, 10), // 4MiB page
        vaddressReg.address(31, 12) === table(i).vpn // 4KiB page
      )
      grants(i) := matchAddress && table(i).valid
    }

    when(grants.reduceTree(_ | _)) { // Hit
      for (i <- 0 until depth) {
        when(grants(i)) {
          when(ptePrivilegeCheck(table(i).uxwr)) {
            // 34 -> 32
            paddressReg.address := Mux(
              table(i).mPage,
              table(i).ppn(21, 10) ## vaddressReg.address(21, 0), // 4MiB page
              table(i).ppn ## vaddressReg.address(11, 0) // 4KiB page
            )
          }.otherwise { // Fault
            error(TLBErrorCode.pageFault)
          }
        }
      }
      statusReg := Status.idle
    }.otherwise { // Miss
      statusReg := Status.vpn1
    }
  }

  private val vpn1Reg = Reg(DataType.operation)

  private def error(errorType: TLBErrorCode.Type) = {
    paddressReg.error := errorType
    statusReg := Status.idle
  }

  private def ptePrivilegeCheck(uxwr: UInt) = {
    val rwxResult = WireInit(false.B)
    val suResult = WireInit(false.B)

    // Check xwr
    when(vaddressReg.write) { // Write and read
      rwxResult := uxwr(1) === 1.U // Writable
    }.otherwise { // Read
      when(io.mstatus(19) === 0.U) { // MXR
        rwxResult := uxwr(0) === 1.U // Must readable
      }.otherwise {
        rwxResult := uxwr(0) === 1.U || // Readable or executable
          uxwr(2) === 1.U
      }
    }

    // Check S/U
    switch(io.privilege) {
      is(PrivilegeType.S) {
        when(io.mstatus(18) === 0.U) { // SUM
          suResult := uxwr(3) === 0.U // Must not user page
        }.otherwise {
          suResult := true.B
        }
      }
      is(PrivilegeType.U) {
        suResult := uxwr(3) === 1.U
      }
    }

    rwxResult && suResult
  }

  private def pteValidCheck(pte: UInt) = {
    pte(0) === 1.U &&
    pte(3, 1) =/= PTERWX.reservedW &&
    pte(3, 1) =/= PTERWX.reservedWX
  }

  private def pteADCheck(pte: UInt) = {
    pte(6) === 1.U && // A is set
    (
      (vaddressReg.write && pte(7) === 1.U) // Write and D is set
        || !vaddressReg.write // Or read only
    )
  }

  private def pteCheck(pte: UInt) = {
    Map(
      "valid" -> pteValidCheck(pte),
      "next" -> (pte(3, 1) === PTERWX.next),
      "privilege" -> ptePrivilegeCheck(pte(4, 1)),
      "misaligned" -> (pte(19, 10) === 0.U),
      "AD" -> pteADCheck(pte)
    )
  }

  when(statusReg === Status.vpn1) {
    // PTE-1 entry address (34 -> 32)
    io.sma.address := io.satp(21, 0) ## vaddressReg.address(31, 22) ## 0.U(2.W)
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) {
        vpn1Reg := io.sma.data
        val pteCheckResult = pteCheck(io.sma.data)
        when(!pteCheckResult("valid")) { // Invalid
          error(TLBErrorCode.pageFault)
        }.elsewhen(pteCheckResult("next")) { // 4KiB page
          statusReg === Status.vpn0
        }.elsewhen(
          !pteCheckResult("misaligned") ||
            !pteCheckResult("AD") ||
            !pteCheckResult("privilege")
        ) { // Fault
          error(TLBErrorCode.pageFault)
        }.otherwise { // OK 4MiB page
          val entry = alloc()
          entry.vpn := vaddressReg.address(31, 12)
          entry.ppn := io.sma.data(31, 10)
          entry.uxwr := io.sma.data(4, 1)
          entry.mPage := true.B
          entry.valid := true.B
          statusReg := Status.lookup // Return
        }
      }.otherwise { // Memory error
        error(TLBErrorCode.accessFault)
      }
    }
  }

  when(statusReg === Status.vpn0) {
    // PTE-2 entry address (34 -> 32)
    io.sma.address := vpn1Reg(31, 10) ## vaddressReg.address(21, 12) ## 0.U(2.W)
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) { // 4KiB page
        val pteCheckResult = pteCheck(io.sma.data)
        when(
          !pteCheckResult("valid") ||
            !pteCheckResult("AD") ||
            !pteCheckResult("privilege") ||
            pteCheckResult("next")
        ) { // Fault
          error(TLBErrorCode.pageFault)
        }.otherwise { // OK 4KiB page
          val entry = alloc()
          entry.vpn := vaddressReg.address(31, 12)
          entry.ppn := io.sma.data(31, 10)
          entry.uxwr := io.sma.data(4, 1)
          entry.mPage := false.B
          entry.valid := true.B
          statusReg := Status.lookup // Return
        }
      }.otherwise { // Memory error
        error(TLBErrorCode.accessFault)
      }
    }
  }
}
