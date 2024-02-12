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

class FPAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPMulEntry())
    val in2 = Input(new FPMulEntry())
    val out = Output(new FPAddEntry())
  })

  // expDiff must be a positive integer
  private val expDiff = (io.in1.exponent - io.in2.exponent).asUInt
  private val shift = io.in2.significand >> expDiff

  private val compare = io.in1.significand >= shift
  private val add = io.in1.significand +& shift // 128 -> 129
  private val sub = Mux(compare, io.in1.significand - shift, shift - io.in1.significand)

  when(io.in1.sign ^ io.in2.sign) {
    io.out.sign := Mux(io.in1.sign, Mux(compare, 1.U, 0.U), Mux(compare, 0.U, 1.U))
    io.out.exponent := io.in1.exponent
    io.out.significand := sub ## 0.U
  }.otherwise {
    io.out.sign := io.in1.sign
    io.out.exponent := io.in1.exponent
    io.out.significand := Mux(add(128), add, add(127, 0) ## 0.U)
  }
}

class FPExpOrder extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val out1 = Output(new FPEntry())
    val out2 = Output(new FPEntry())
  })
  private val compare = io.in1.exponent >= io.in2.exponent

  io.out1 := Mux(compare, io.in1, io.in2)
  io.out2 := Mux(compare, io.in2, io.in1)
}
