package lltriscv.cache

import chisel3._
import chisel3.util._

class FlushCacheIO extends Bundle {
  val req = Output(Bool())
  val empty = Input(Bool())
}
