package lltriscv.peripheral

import chisel3._
import chisel3.util._

import lltriscv.bus.AXIMasterIO

import lltriscv.utils.ChiselUtils._

class VirtualRO(base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
    val roAddress = Output(UInt(32.W))
    val roData = Input(UInt(32.W))
  })

  io.axi <> new AXIMasterIO().zero

  private val addressReg = RegInit(0.U(32.W))

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    addressReg := io.axi.ARADDR - base.U
  }

  io.roAddress := addressReg

  io.axi.RVALID := true.B
  io.axi.RDATA := io.roData
  io.axi.RRESP := 0.U

  // Disable write
  io.axi.AWREADY := true.B
  io.axi.WREADY := true.B
  io.axi.BVALID := true.B
  io.axi.BRESP := "b10".U
}
