package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._
import bergamot.utils.ReadWriteSRAM

class MemoryHole extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
  })
  io.axi.ARREADY := true.B

  io.axi.RRESP := "b11".U
  io.axi.RDATA := 0.U
  io.axi.RVALID := true.B

  io.axi.AWREADY := true.B

  io.axi.WREADY := true.B

  io.axi.BRESP := "b11".U
  io.axi.BVALID := true.B
}
