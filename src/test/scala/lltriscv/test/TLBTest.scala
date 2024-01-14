package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import chisel3.util._
import lltriscv.core.record.DataTLB
import lltriscv.test.mock.FlatMemoryMock
import lltriscv.test.mock.MemoryMocks
import lltriscv.test.mock.MapMemoryMock
import lltriscv.utils.ChiselUtils

class TBLTest extends AnyFreeSpec with ChiselScalatestTester {
  "4Kib page translation should be successful" in {
    test(new DataTLB(32)).withAnnotations(
      Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    ) { dut =>
      dut.clock.step()
      val memory = new MapMemoryMock()
      memory.storeInt(
        28,
        ChiselUtils.BinaryString2Int("000000000000_0000000001_00_0000_000_1")
      ) // 0 page 7 PTE

      memory.storeInt(
        4116,
        ChiselUtils.BinaryString2Int("000000000000_0000000101_00_0000_111_1")
      ) // 1 page 5 PTE

      var exit = false
      while (!exit) {
        MemoryMocks.smaMock(dut.io.sma, dut.clock, memory)
        dut.io.vaddress.valid.poke(true.B)
        dut.io.paddress.ready.poke(true.B)
        dut.io.vaddress.bits
          .poke("b0000000111_0000000101_000000000100".U) // 7 - 5
        if (
          dut.io.paddress.valid.peekBoolean() &&
          !dut.io.paddress.bits.error.peekBoolean() &&
          dut.io.paddress.bits.address.peekInt() ==
            ChiselUtils.BinaryString2Int("0000000000_0000000101_000000000100")
        ) exit = true
        dut.clock.step()
      }
    }
  }
}
