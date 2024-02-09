package bergamot.cache

import chisel3._
import chisel3.util._

import bergamot.core.DataType
import bergamot.core.CoreConstant

/*
 * Cache entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Flush cache interface
  *
  * Request to refresh Cache, there are different implementations for different caches.
  */
class FlushCacheIO extends Bundle {
  val req = Output(Bool()) // Reqeust
  val empty = Input(Bool()) // Empty
}

/** Instruction cache line request interface
  *
  * Request data for an instruction cache line
  *
  * @param cacheLineDepth
  *   Size(bits) = 16 * Cache line depth
  */
class CacheLineRequestIO(cacheLineDepth: Int) extends Bundle {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Require 32-bit aligned cache line depth")

  val address = Output(DataType.address) // Aligned cache line address
  val data = Input(Vec(cacheLineDepth, UInt(CoreConstant.cacheCellLength.W))) // Group data
  val error = Input(Bool()) // Memory error
  val valid = Output(Bool())
  val ready = Input(Bool())
}
