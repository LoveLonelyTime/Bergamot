package lltriscv.test

import chisel3._
import chiseltest._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec

import lltriscv.core.LLTRISCVCoreExq
import lltriscv.core.CoreConfig

import lltriscv.test.mock.FlatMemoryMock
import lltriscv.test.mock.MemoryFileMock
import lltriscv.test.mock.SMAMemoryMock
import java.io.File
import lltriscv.test.mock.MemoryMock
import lltriscv.peripheral.RAM
import lltriscv.interconnect.AXIInterconnect
import lltriscv.peripheral.ROM

class NormalTest extends AnyFlatSpec with ChiselScalatestTester {
  private val testAnnotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  private val config = CoreConfig.default

  private class TestCore extends Module {
    val io = IO(new Bundle {})

    private val core = Module(new LLTRISCVCoreExq(config))

    // private val interconnect = Module(
    //   new AXIInterconnect(
    //     Seq(
    //       "h00000000",
    //       "h80000000"
    //     )
    //   )
    // )
    private val rom = Module(new ROM(1024, "test.hex"))
    // private val ram = Module(new RAM(1024))

    // interconnect.io.slaves(0) <> rom.io.axi
    // interconnect.io.slaves(1) <> ram.io.axi

    // core.io.axi <> interconnect.io.master
    core.io.axi <> rom.io.axi
  }

  "Normal test" should "pass" in {
    emitVerilog(new TestCore(), Array("--target-dir", "generated"))
    test(new TestCore()).withAnnotations(testAnnotations) { dut =>
      dut.clock.step(500)
    }
  }
}
