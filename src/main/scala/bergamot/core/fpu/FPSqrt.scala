package bergamot.core.fpu

import chisel3._
import chisel3.util._

class FPSqrt extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val out = Output(new FPAddEntry())
  })

  // Restoring shift/subtract algorithm
  private val remainders = Wire(Vec(io.in1.significand.getWidth, UInt((io.in1.significand.getWidth * 2 + 2).W)))
  private val roots = Wire(Vec(io.in1.significand.getWidth, UInt((io.in1.significand.getWidth * 2).W)))
  remainders(0) := io.in1.significand ## Fill(io.in1.significand.getWidth, 0.U)
  roots(0) := 0.U

  for (i <- 1 to io.in1.significand.getWidth * 2) {
    val r2 = remainders(i - 1) ## 0.U // x2
    val term = roots(i - 1).head(i - 1) ## 0.U ## 1.U ## 0.U((io.in1.significand.getWidth * 2 - i).W) // x2 + 1
    when(r2 >= term) { // 1
      remainders(i) := r2 - term
      roots(i) := roots(i - 1).head(i - 1) ## 1.U ## 0.U((io.in1.significand.getWidth * 2 - i).W)
    }.otherwise { // 0
      remainders(i) := r2
      roots(i) := roots(i - 1).head(i - 1) ## 0.U ## 0.U((io.in1.significand.getWidth * 2 - i).W)
    }
  }

}
