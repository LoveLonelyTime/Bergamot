package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.execute.ExceptionCode

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

class ExceptionRequestIO extends Bundle{
  val xret = Output(Bool())
  val trigger = Output(Bool())
  val exceptionPC = Output(DataType.address)
  val exceptionCode = Output(DataType.exceptionCode)
  val exceptionVal = Output(DataType.operation)
  val handlerPC = Input(DataType.address)
}