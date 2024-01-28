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

/*
 * This test case is used to export Verilog files.
 *
 * By default configuration, and output directory is `generated`
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

class ExportCoreTest extends AnyFreeSpec with ChiselScalatestTester {
  class TestCore extends Module {
    val io = IO(new Bundle {
      val readone = Input(UInt(4.W))
      val writeone = Output(Bool())

      val readReady = Input(Bool())
      val writeReady = Input(Bool())
      val readValid = Output(Bool())
      val writeValid = Output(Bool())
    })
    private val core = Module(new LLTRISCVCoreExq(CoreConfig.default))
    core.io.smaReader.ready := io.readReady
    core.io.smaReader.error := false.B
    core.io.smaReader.data := Fill(8, io.readone)

    io.readValid := core.io.smaReader.valid

    core.io.smaWriter.ready := io.writeReady
    core.io.smaWriter.error := false.B
    io.writeone := core.io.smaWriter.data.xorR

    io.writeValid := core.io.smaWriter.valid
  }

  "Export core" in {
    emitVerilog(new TestCore(), Array("--target-dir", "generated"))
  }
}
