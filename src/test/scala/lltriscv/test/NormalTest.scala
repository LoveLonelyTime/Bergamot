package bergamot.test

import chisel3._
import chiseltest._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec

import bergamot.core.BergamotCore
import bergamot.core.CoreConfig

import bergamot.test.mock.FlatMemoryMock
import bergamot.test.mock.MemoryFileMock
import bergamot.test.mock.SMAMemoryMock
import java.io.File
import bergamot.test.mock.MemoryMock
import bergamot.peripheral.RAM
import bergamot.interconnect.AXIInterconnect
import bergamot.peripheral.ROM
import java.io.RandomAccessFile
import bergamot.utils.ChiselUtils
import bergamot.peripheral.VirtualWriteHost
import bergamot.peripheral.VirtualUART
import bergamot.peripheral.MemoryHole
import bergamot.peripheral.VirtualRAM
import bergamot.peripheral.MachineTimer
import bergamot.core.debug.DebugIO

class NormalTest extends AnyFlatSpec with ChiselScalatestTester {
  // WriteVcdAnnotation
  private val testAnnotations = Seq(VerilatorBackendAnnotation)

  private val config = CoreConfig.default.copy(pcInit = "hffff0000")

  private class TestCore extends Module {
    val io = IO(new Bundle {
      val rdAddress = Output(UInt(32.W))
      val rdData = Input(UInt(32.W))
      val wrAddress = Output(UInt(32.W))
      val wrData = Output(UInt(32.W))
      val wrStrobe = Output(UInt(4.W))

      val dataOut = Output(UInt(8.W))
      val send = Output(Bool())

      val debug = new DebugIO()
    })

    private val core = Module(new BergamotCore(config))

    private val interconnect = Module(
      new AXIInterconnect(
        Seq(
          "h00000000", // hole
          "h2000000", // mtime
          "h10000000", // uart
          "h80000000", // ram
          "hffff0000" // rom
        )
      )
    )
    private val hole = Module(new MemoryHole())
    private val uart = Module(new VirtualUART("h10000000"))
    private val mtimer = Module(new MachineTimer("h2000000"))
    private val rom = Module(new ROM(32, "hffff0000", "boot.hex"))
    private val ram = Module(new VirtualRAM("h80000000"))

    io.rdAddress := ram.io.rdAddress
    ram.io.rdData := io.rdData
    io.wrAddress := ram.io.wrAddress
    io.wrData := ram.io.wrData
    io.wrStrobe := ram.io.wrStrobe

    io.dataOut := uart.io.dataOut
    io.send := uart.io.send

    interconnect.io.slaves(0) <> hole.io.axi
    interconnect.io.slaves(1) <> mtimer.io.axi
    interconnect.io.slaves(2) <> uart.io.axi
    interconnect.io.slaves(3) <> ram.io.axi
    interconnect.io.slaves(4) <> rom.io.axi

    core.io.mtime := mtimer.io.mtime
    core.io.mtimeIRQ := mtimer.io.irq
    core.io.axi <> interconnect.io.master

    core.io.debug <> io.debug
  }

  "Normal test" should "pass" in {
    // 4MB
    emitVerilog(new TestCore(), Array("--target-dir", "sim"))
    // val roSize = 1024 * 1024 * 4
    // val memory = new FlatMemoryMock(roSize) with MemoryFileMock
    // memory.importBin(new File("fw_jump.bin"), 0)
    // test(new TestCore()).withAnnotations(testAnnotations) { dut =>
    //   dut.clock.setTimeout(0)
    //   for (i <- 0 until 5000000) {
    //     if (i % 10000 == 0) {
    //       println(s"Clock:$i")
    //     }
    //     dut.io.roData.poke(ChiselUtils.int2UInt(memory.loadInt(dut.io.roAddress.peekInt().toInt)))
    //     dut.clock.step()
    //   }
    // }
  }
}
