package bergamot.core.fpu

import chisel3._
import chisel3.util._

class FPDiv extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val out = Output(new FPAddEntry())
  })

  io.out.exponent := io.in1.exponent - io.in2.exponent + io.in1.significand.getWidth.S
  io.out.significand := (io.in1.significand ## Fill(io.in1.significand.getWidth, 0.U) / io.in2.significand) ## 0.U // 64 -> 129
  io.out.sign := io.in1.sign ^ io.in2.sign
}
