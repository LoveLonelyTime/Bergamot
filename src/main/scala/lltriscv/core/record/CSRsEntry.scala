package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._

class CSRsWriteIO extends Bundle {
  val address = Output(DataType.csr)
  val data = Output(DataType.operation)
  val wen = Output(Bool())
}
