package lltriscv.peripheral

import chisel3._
import chisel3.util._

import lltriscv.bus.AXIMasterIO

import lltriscv.utils.ChiselUtils._
import lltriscv.utils.CoreUtils._
import lltriscv.utils.ReadWriteSRAM

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
