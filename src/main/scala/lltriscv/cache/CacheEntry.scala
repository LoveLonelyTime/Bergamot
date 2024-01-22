package lltriscv.cache

import chisel3._
import chisel3.util._

import lltriscv.core.DataType

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
class ICacheLineRequestIO(cacheLineDepth: Int) extends Bundle {
  val address = Output(DataType.address) // Aligned cache line address
  val data = Input(Vec(cacheLineDepth, UInt(16.W))) // Group data
  val error = Input(Bool()) // Memory error
  val valid = Output(Bool())
  val ready = Input(Bool())
}
