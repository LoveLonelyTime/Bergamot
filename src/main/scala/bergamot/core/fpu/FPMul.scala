package bergamot.core.fpu

import chisel3._
import chisel3.util._

class FPMul extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val out = Output(new FPMulEntry())
  })

  io.out.significand := io.in1.significand * io.in2.significand // 64 -> 128
  io.out.exponent := io.in1.exponent + io.in2.exponent
  io.out.sign := io.in1.sign ^ io.in2.sign
}

class FPMulAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val in3 = Input(new FPEntry())
    val out = Output(new FPAddEntry())
  })

  private val fpMul = Module(new FPMul())
  fpMul.io.in1 := io.in1
  fpMul.io.in2 := io.in2

  private val addend2 = Wire(new FPMulEntry())
  FP.extend(io.in3, addend2)

  private val fpAdd = Module(new FPAdd())
  fpAdd.io.in1 := fpMul.io.out
  fpAdd.io.in2 := addend2

  io.out := fpAdd.io.out
}
