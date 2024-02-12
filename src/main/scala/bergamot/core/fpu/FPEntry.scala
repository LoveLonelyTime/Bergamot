package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.core.DataType
import bergamot.core.CoreConstant

trait IEEESpec {
  val width: Int
  val signWidth: Int
  val significandWidth: Int
  val exponentWidth: Int

  val bias = (1 << (exponentWidth - 1)) - 1
  val minExp = -bias + 1
  val maxExp = bias
}

object IEEESpec32 extends IEEESpec {
  val width: Int = 32
  val signWidth: Int = 1
  val significandWidth: Int = 8
  val exponentWidth: Int = 23
}

object IEEESpec64 extends IEEESpec {
  val width: Int = 64
  val signWidth: Int = 1
  val significandWidth: Int = 11
  val exponentWidth: Int = 52
}

object FPRoundoff extends ChiselEnum {
  val RNE, RTZ, RDN, RUP, RMM = Value

  def roundoffModes(fp: FPAddEntry, reserved: Int) = {
    Seq(
      RNE -> roundTiesToEven(fp, reserved)
    )
  }

  def up(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val add = fp.significand.head(reserved) +& 1.U

    result.sign := fp.sign
    when(add(reserved)) {
      result.exponent := fp.exponent + 1.S
      result.significand := add(reserved, 1) ## 0.U((result.significand.getWidth - reserved).W)
    }.otherwise {
      result.exponent := fp.exponent
      result.significand := add(reserved - 1, 0) ## 0.U((result.significand.getWidth - reserved).W)
    }

    result
  }

  def down(fp: FPAddEntry, reserved: Int) = {
    val result = WireInit(fp)
    result.significand := fp.significand.head(reserved) ## 0.U((result.significand.getWidth - reserved).W)

    result
  }

  def roundTiesToEven(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val judge = fp.significand.head(reserved + 1)(1, 0) ## !fp.significand.tail(fp.significand.getWidth - reserved - 1).orR
    val upVal = up(fp, reserved)
    val downVal = down(fp, reserved)

    // LSB | Drop MSB | Half
    result := MuxLookup(judge, fp)(
      Seq(
        // Drop MSB is 0
        "b000".U -> downVal,
        "b100".U -> downVal,
        "b001".U -> downVal,
        "b101".U -> downVal,
        // Drop MSB is 1, but not half
        "b010".U -> upVal,
        "b110".U -> upVal,
        // Drop MSB is 1 and half
        "b011".U -> downVal,
        "b111".U -> upVal
      )
    )

    result
  }
}

class FPEntry extends Bundle {
  val sign = Bool()
  val significand = DataType.double
  val exponent = SInt(CoreConstant.halfWidth.W)
}

class FPMulEntry extends Bundle {
  val sign = Bool()
  val significand = UInt((DataType.double.getWidth * 2).W)
  val exponent = SInt(CoreConstant.halfWidth.W)
}

class FPAddEntry extends Bundle {
  val sign = Bool()
  val significand = UInt((DataType.double.getWidth * 2 + 1).W)
  val exponent = SInt(CoreConstant.halfWidth.W)
}
