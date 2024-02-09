package bergamot.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import chisel3.util._
import bergamot.core.CoreConfig
import bergamot.core.BergamotCore
import bergamot.test.mock.FlatMemoryMock
import bergamot.test.mock.MemoryFileMock
import java.io.File
import bergamot.core.execute.MemoryAccessLength
import bergamot.utils.ChiselUtils

/*
 * This test case is used to export Verilog files.
 *
 * By default configuration, and output directory is `generated`
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

class ExportCoreTest extends AnyFreeSpec with ChiselScalatestTester {
  val config = CoreConfig(
    iTLBDepth = 1,
    cacheLineDepth = 8,
    fetchQueueDepth = 1,
    executeQueueWidth = 3,
    executeQueueDepth = 1,
    dTLBDepth = 1,
    storeQueueDepth = 1,
    robDepth = 1,
    predictorDepth = 1,
    pcInit = "h00000000",
    l1CacheDepth = 4,
    l1CacheWay = 1,
    l2CacheDepth = 4,
    l2CacheWay = 1,
    memoryAddress = "h80000000"
  )
  "Export core" in {
    emitVerilog(new BergamotCore(config), Array("--target-dir", "generated"))
  }
}
