package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._

object PrivilegeType extends ChiselEnum {
  val M, S, U = Value
}

class CSRsWriteIO extends Bundle {
  val address = Output(DataType.csr)
  val data = Output(DataType.operation)
  val wen = Output(Bool())
}

class CSRsReadIO extends Bundle {
  val address = Input(DataType.csr)
  val data = Output(DataType.operation)
  val error = Output(Bool())
}
