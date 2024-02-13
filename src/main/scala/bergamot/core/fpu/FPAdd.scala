package bergamot.core.fpu

import chisel3._
import chisel3.util._

class FPAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPMulEntry())
    val in2 = Input(new FPMulEntry())
    val out = Output(new FPAddEntry())
  })

  private val expCompare = io.in1.exponent >= io.in2.exponent
  private val addend1 = Mux(expCompare, io.in1, io.in2)
  private val addend2 = Mux(expCompare, io.in2, io.in1)

  // expDiff must be a positive integer

  private val expDiff = (addend1.exponent - addend2.exponent).asUInt
  private val shift = addend2.significand >> expDiff

  private val compare = addend1.significand >= shift
  private val add = addend1.significand +& shift // 128 -> 129
  private val sub = Mux(compare, addend1.significand - shift, shift - addend1.significand)

  when(addend1.sign ^ addend2.sign) {
    io.out.sign := Mux(addend1.sign, Mux(compare, 1.U, 0.U), Mux(compare, 0.U, 1.U))
    io.out.exponent := addend1.exponent
    io.out.significand := sub ## 0.U
  }.otherwise {
    io.out.sign := addend1.sign
    io.out.exponent := addend1.exponent
    io.out.significand := Mux(add.head(1).asBool, add, add.tail(1) ## 0.U)
  }
}
