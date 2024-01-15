package lltriscv.bus

import chisel3._
import chisel3.util._

/*
 * SMA (Simple Memory Access) interface
 */

object SMASpec {
  val addressWidth = 32
  val dataWidth = 32
}

class SMAReaderIO extends Bundle {
  val address = Output(UInt(SMASpec.addressWidth.W))
  val data = Input(UInt(SMASpec.dataWidth.W))
  val valid = Output(Bool())
  val ready = Input(Bool())
  val error = Input(Bool())
}

class SMAReaderWriterIO extends Bundle {
  val address = Output(UInt(SMASpec.addressWidth.W))
  val wen = Output(Bool())
  val wdata = Output(UInt(SMASpec.dataWidth.W))
  val rdata = Input(UInt(SMASpec.dataWidth.W))
  val valid = Output(Bool())
  val ready = Input(Bool())
  val error = Input(Bool())
}
