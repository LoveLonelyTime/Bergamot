package lltriscv.cache

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

class FlushCacheIO extends Bundle {
  val req = Output(Bool())
  val empty = Input(Bool())
}

class ICacheLineRequestIO(cacheLineDepth: Int) extends Bundle {
  val address = Output(DataType.address)
  val data = Input(Vec(cacheLineDepth, UInt(16.W)))
  val error = Input(Bool())
  val valid = Output(Bool())
  val ready = Input(Bool())
}
