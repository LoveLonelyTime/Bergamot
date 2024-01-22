package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import chisel3.util._
import lltriscv.core.CoreConfig
import lltriscv.core.LLTRISCVCoreExq
import lltriscv.test.mock.FlatMemoryMock
import lltriscv.test.mock.MemoryFileMock
import java.io.File
import lltriscv.core.execute.MemoryAccessLength
import lltriscv.utils.ChiselUtils

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "Print verilog" in {
    emitVerilog(new LLTRISCVCoreExq(CoreConfig.default), Array("--target-dir", "generated"))
  }

  "RegisterMappingTable should be OK" in {
    test(new LLTRISCVCoreExq(CoreConfig.default)).withAnnotations(
      Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    ) { dut =>
      val memory = new FlatMemoryMock(4096 * 4) with MemoryFileMock
      memory.importBin(new File("boot.bin"), 0)
      memory.importBin(new File("test.bin"), 12288)
      var run = true
      // for (i <- 0 until 500) {
      while (run) {
        // Read write memory
        dut.io.smaReader.ready.poke(false.B)
        dut.io.smaReader.error.poke(false.B)
        dut.io.smaWriter.ready.poke(false.B)
        dut.io.smaWriter.error.poke(false.B)
        if (dut.io.smaReader.valid.peekBoolean()) {
          dut.io.smaReader.ready.poke(true.B)
          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.byte) {
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadByte(dut.io.smaReader.address.peekInt().toInt)))
          }

          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.half) {
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadShort(dut.io.smaReader.address.peekInt().toInt)))
          }

          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.word) {
            // val addr = dut.io.smaReader.address.peekInt().toInt
            // println(s"Reader addr: ${addr}, data: ${memory.loadInt(addr)}")
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadInt(dut.io.smaReader.address.peekInt().toInt)))
          }
        } else if (dut.io.smaWriter.valid.peekBoolean()) {
          dut.io.smaWriter.ready.poke(true.B)

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.byte) {
            memory.storeByte(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()).toByte)
          }

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.half) {
            memory.storeShort(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()).toShort)
          }

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.word) {
            val addr = dut.io.smaWriter.address.peekInt().toInt
            // println(s"Writer addr: ${addr}, data: ${dut.io.smaWriter.data.peekInt()}")
            if (dut.io.smaWriter.address.peekInt().toInt == 12412) {
              run = false
              println(s"Writer addr: ${addr}, data: ${dut.io.smaWriter.data.peekInt()}")
            }
            // if (dut.io.smaWriter.address.peekInt().toInt == 124) { run = false }
            memory.storeInt(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()))
          }
        }
        dut.clock.step()
      }
    }
  }
}
