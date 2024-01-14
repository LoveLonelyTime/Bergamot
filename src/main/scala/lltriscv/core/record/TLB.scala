package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.bus.SMAReaderIO
import lltriscv.utils.CoreUtils

// Sv32

// TODO: Authentication
class DataTLB(depth: Int) extends Module {
  val io = IO(new Bundle {
    val vaddress = Flipped(DecoupledIO(DataType.address))
    val paddress = DecoupledIO(new TLBPAddressEntry())

    // SMA interface
    val sma = new SMAReaderIO()

    // satp
    val satp = Input(DataType.operation)
  })

  // TLB
  private val table = Reg(Vec(depth, new TLBEntry()))

  private val vaddressReg = Reg(DataType.address)
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
    statusReg := Status.lookup
  }
  io.sma.valid := false.B
  io.sma.address := 0.U

  when(statusReg === Status.lookup) {
    // Fully-associative
    val grants = VecInit.fill(depth)(false.B)
    for (i <- 0 until depth) {
      val matchAddress = Mux(
        table(i).mPage,
        vaddressReg(31, 22) === table(i).vpn(19, 10), // 4MiB page
        vaddressReg(31, 12) === table(i).vpn // 4KiB page
      )
      grants(i) := matchAddress && table(i).valid
    }

    when(grants.reduceTree(_ | _)) { // Hit
      for (i <- 0 until depth) {
        when(grants(i)) {
          // 34 -> 32
          paddressReg.address := Mux(
            table(i).mPage,
            table(i).ppn(21, 10) ## vaddressReg(21, 0), // 4MiB page
            table(i).ppn ## vaddressReg(11, 0) // 4KiB page
          )
          paddressReg.error := table(i).error
        }
      }
      statusReg := Status.idle
    }.otherwise { // Miss
      statusReg := Status.vpn1
    }
  }

  private val vpn1Reg = Reg(DataType.operation)

  when(statusReg === Status.vpn1) {
    // PTE-1 entry address (34 -> 32)
    io.sma.address := io.satp(21, 0) ## vaddressReg(31, 22) ## 0.U(2.W)
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) {
        vpn1Reg := io.sma.data
        when(io.sma.data(3, 1) === 0.U) { // 4KiB page
          statusReg := Status.vpn0
        }.otherwise { // 4MiB page
          val entry = alloc()
          entry.vpn := vaddressReg(31, 12)
          entry.ppn := io.sma.data(31, 10)
          entry.mPage := true.B
          entry.error := false.B
          entry.valid := true.B
          statusReg := Status.lookup // Return
        }
      }.otherwise { // Memory error
        val entry = alloc()
        entry.vpn := vaddressReg(31, 12)
        entry.mPage := true.B
        entry.error := true.B
        entry.valid := true.B
        statusReg := Status.lookup // Return
      }
    }
  }

  when(statusReg === Status.vpn0) {
    // PTE-2 entry address (34 -> 32)
    io.sma.address := vpn1Reg(31, 10) ## vaddressReg(21, 12) ## 0.U(2.W)
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) { // 4KiB page
        val entry = alloc()
        entry.vpn := vaddressReg(31, 12)
        entry.ppn := io.sma.data(31, 10)
        entry.mPage := false.B
        entry.error := false.B
        entry.valid := true.B
        statusReg := Status.lookup // Return
      }.otherwise { // Memory error
        val entry = alloc()
        entry.vpn := vaddressReg(31, 12)
        entry.mPage := false.B
        entry.error := true.B
        entry.valid := true.B
        statusReg := Status.lookup // Return
      }
    }
  }
}
