package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

class FPConv2FP(width: Int, sign: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(new FPAddEntry())
  })

  private val ext = Wire(UInt(io.out.significand.getWidth.W))
  ext := io.in

  io.out.exponent := io.out.significand.getWidth.S
  io.out.fpType := FPType.normal
  if (sign) {
    when(io.in.head(1).asBool) { // Negitive
      io.out.significand := ~signExtended(ext, io.in.getWidth - 1) + 1.U
      io.out.sign := 1.U
    }.otherwise { // Positive
      io.out.significand := ext
      io.out.sign := 0.U
    }
  } else {
    io.out.significand := ext
    io.out.sign := 0.U
  }
}

// class FPConvFP2 extends Module {
//   val io = IO(new Bundle {
//     val in = Input(UInt(width.W))
//     val out = Output(new FPAddEntry())
//   })
// }
