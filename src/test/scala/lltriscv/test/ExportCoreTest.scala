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
  "Export core" in {
    emitVerilog(new LLTRISCVCoreExq(CoreConfig.default), Array("--target-dir", "generated"))
  }
}
