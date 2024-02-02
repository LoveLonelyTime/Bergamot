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
import lltriscv.peripheral.VirtualRO
import java.io.RandomAccessFile
import lltriscv.utils.ChiselUtils
import lltriscv.peripheral.VirtualWriteHost

class NormalTest extends AnyFlatSpec with ChiselScalatestTester {
  private val testAnnotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  private val config = CoreConfig.default.copy(pcInit = "hffff0000")

  private class TestCore extends Module {
    val io = IO(new Bundle {
      val roAddress = Output(UInt(32.W))
      val roData = Input(UInt(32.W))
      val accept = Output(Bool())
    })

    private val core = Module(new LLTRISCVCoreExq(config))

    private val interconnect = Module(
      new AXIInterconnect(
        Seq(
          "h00000000", // writeHost
          "h80000000", // ram
          "hf0000000", // ro
          "hffff0000" // rom
        )
      )
    )
    private val rom = Module(new ROM(1024, "hffff0000", "boot.hex"))
    private val ram = Module(new RAM(4096 * 10, "h80000000"))
    private val ro = Module(new VirtualRO("hf0000000"))
    private val writeHost = Module(new VirtualWriteHost("h00000000"))
    io.roAddress := ro.io.roAddress
    ro.io.roData := io.roData

    io.accept := writeHost.io.accept

    interconnect.io.slaves(0) <> writeHost.io.axi
    interconnect.io.slaves(1) <> ram.io.axi
    interconnect.io.slaves(2) <> ro.io.axi
    interconnect.io.slaves(3) <> rom.io.axi

    core.io.axi <> interconnect.io.master
  }

  "Normal test" should "pass" in {
    val roSize = 4096 * 2
    val memory = new FlatMemoryMock(roSize) with MemoryFileMock
    memory.importBin(new File("test.bin"), 0)
    test(new TestCore()).withAnnotations(testAnnotations) { dut =>
      dut.clock.setTimeout(0)
      for (i <- 0 until 100000) {
        dut.io.roData.poke(ChiselUtils.int2UInt(memory.loadInt(dut.io.roAddress.peekInt().toInt)))
        dut.clock.step()
      }
      dut.io.accept.expect(true)
    }
  }
}
