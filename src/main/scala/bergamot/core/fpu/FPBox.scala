package bergamot.core.fpu

import chisel3._
import chisel3.util._
import bergamot.core.DataType

class FPWordUnbox extends Module {
  val io = IO(new Bundle {
    val in = Input(DataType.word)
    val out = Output(new FPEntry())
  })

  when(io.in(30, 23).andR) { // NaN
    // TODO
    io.out.sign := 0.U
    io.out.significand := 0.U
    io.out.exponent := 0.U
  }.elsewhen(!io.in(30, 23).orR) { // Subnormal number
    io.out.sign := io.in(31)
    io.out.significand := 0.U ## io.in(22, 0) ## 0.U(41.W)
    io.out.exponent := -126.S
  }.otherwise { // Normal number
    io.out.sign := io.in(31)
    io.out.significand := 1.U ## io.in(22, 0) ## 0.U(41.W)
    io.out.exponent := io.in(30, 23).asSInt - 127.S
  }
}

class FPBox(spec: IEEESpec) extends Module {
  val io = IO(new Bundle {
    val in = Input(new FPAddEntry())
    val out = Output(UInt(spec.width.W))
    val roundoff = Input(FPRoundoff())
  })

  // Alignment
  private val alignment = WireInit(io.in)

  for (i <- 0 until io.in.significand.getWidth) {
    when(io.in.significand(i)) {
      val filler = alignment.significand.getWidth - i - 1
      alignment.significand := io.in.significand(i, 0) ## 0.U(filler.W)
      alignment.exponent := io.in.exponent - (filler).S
    }
  }

  // Roundoff
  private val subnormalRound = MuxLookup(io.roundoff, alignment)(FPRoundoff.roundoffModes(alignment, spec.significandWidth))
  private val normalRound = MuxLookup(io.roundoff, alignment)(FPRoundoff.roundoffModes(alignment, spec.significandWidth + 1))

  // To IEEE745
  private val biasExp = (normalRound.exponent + spec.bias.S).tail(spec.exponentWidth)

  when(subnormalRound.exponent < spec.minExp.S) { // -> 0
    io.out := subnormalRound.sign ## 0.U(spec.exponentWidth.W) ## 0.U(spec.significandWidth.W)
  }.elsewhen(subnormalRound.exponent === spec.minExp.S) { // Subnormal number
    io.out := subnormalRound.sign ## 0.U(spec.exponentWidth.W) ## subnormalRound.significand.head(spec.significandWidth)
  }.elsewhen(normalRound.exponent > (spec.maxExp + 1).S) { // -> inf
    io.out := normalRound.sign ## Fill(spec.exponentWidth, 1.U) ## 0.U(spec.significandWidth.W)
  }.otherwise { // Normal number
    io.out := normalRound.sign ## biasExp.asUInt ## normalRound.significand.head(spec.significandWidth + 1).tail(spec.significandWidth)
  }
}
