package lltriscv.test.riscvtests

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

/*
 * The riscv-tests ISA rv32-p test suite
 *
 * > p: virtual memory is disabled, only core 0 boots up
 *
 * Include: rv32ui, rv32uc
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

class RV32PTest extends AnyFlatSpec with ChiselScalatestTester {
  private val testAnnotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
  private val loadAddress = 0x1000
  private val entryAddress = 0x2000
  private val hostAddress = 0x1000
  private val passTestNum = 0x1
  private val timeout = 100000
  private val memorySize = 1024 * 1024 * 4

  private val config = CoreConfig.default.copy(pcInit = entryAddress)

  // Collect tests
  private val mipTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32mi-p-.*\.bin"))
  private val sipTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32si-p-.*\.bin"))

  private val uipTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32ui-p-.*\.bin"))
  private val ucpTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32uc-p-.*\.bin"))
  private val uapTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32ua-p-.*\.bin"))
  private val umpTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32um-p-.*\.bin"))

  private val uivTests = new File("riscv-tests/isa").listFiles().filter(_.getName().matches(raw"rv32ui-v-.*\.bin"))

  private val needToTest = uivTests

  private def expectPass(memory: MemoryMock) = {
    assert(memory.loadInt(hostAddress) == passTestNum)
  }

  for (bin <- needToTest) {
    s"$bin" should "pass" in {
      test(new LLTRISCVCoreExq(config)).withAnnotations(testAnnotations) { dut =>
        dut.clock.setTimeout(0)
        val memory = new FlatMemoryMock(memorySize) with MemoryFileMock with SMAMemoryMock
        memory.importBin(bin, loadAddress)
        for (i <- 0 until timeout) {
          memory.doMemory(dut.io.smaReader, dut.io.smaWriter)
          dut.clock.step()
        }
        expectPass(memory)
      }
    }
  }
}
