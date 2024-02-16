package bergamot.core.fpu

import chisel3._
import chisel3.util._

class FPComp extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())

    val eq = Output(Bool())
    val lt = Output(Bool())
  })
  io.eq := io.in1 === io.in2

}
