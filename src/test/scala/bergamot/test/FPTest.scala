package bergamot.test

import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chiseltest._
import bergamot.core.fpu.FPUnbox
import bergamot.core.fpu.IEEESpec32
import bergamot.core.fpu.FPMulAdd
import bergamot.core.fpu.FPBox
import bergamot.core.fpu.FPRoundoff
import bergamot.core.fpu.FPDiv

class TestFPMulAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val in3 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  private val fpMulAdd = Module(new FPMulAdd())
  private val fpUnbox1 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox2 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox3 = Module(new FPUnbox(IEEESpec32))
  private val fpBox = Module(new FPBox(IEEESpec32))
  fpUnbox1.io.in := io.in1
  fpUnbox2.io.in := io.in2
  fpUnbox3.io.in := io.in3

  fpMulAdd.io.in1 := fpUnbox1.io.out
  fpMulAdd.io.in2 := fpUnbox2.io.out
  fpMulAdd.io.in3 := fpUnbox3.io.out

  fpBox.io.in := fpMulAdd.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class TestFPDiv extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  private val fpDiv = Module(new FPDiv())
  private val fpUnbox1 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox2 = Module(new FPUnbox(IEEESpec32))
  private val fpBox = Module(new FPBox(IEEESpec32))
  fpUnbox1.io.in := io.in1
  fpUnbox2.io.in := io.in2

  fpDiv.io.in1 := fpUnbox1.io.out
  fpDiv.io.in2 := fpUnbox2.io.out

  fpBox.io.in := fpDiv.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class FPTest extends AnyFlatSpec with ChiselScalatestTester {
  "FPTest" should "pass" in {
    test(new TestFPDiv) { dut =>
      dut.io.in1.poke("h3f800000".U)
      dut.io.in2.poke("h00400000".U)
      dut.clock.step()

      println(dut.io.out.peekInt())
    }
  }
}
