package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.execute.ExceptionCode

/*
 * CSRs entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Privilege type
  *
  * RISC-V privilege
  */
object PrivilegeType extends ChiselEnum {
  /*
   * M: Machine-Level
   * S: Supervisor-Level
   * U: User-Level
   */
  val M, S, U = Value

  def mcode(code: UInt) = {
    val result = WireInit(U)
    switch(code) {
      is("b00".U) { result := U }
      is("b01".U) { result := S }
      is("b11".U) { result := M }
    }
    result
  }
}

/** CSRs write intreface
  */
class CSRsWriteIO extends Bundle {
  val address = Output(DataType.csr) // CSR address
  val data = Output(DataType.operation) // Data
  val wen = Output(Bool())
}

/** CSRs read intreface
  */
class CSRsReadIO extends Bundle {
  val address = Input(DataType.csr) // CSR address
  val data = Output(DataType.operation) // Data
  val error = Output(Bool()) // CSR does not exist
}

/** Exception request interface
  */
class ExceptionRequestIO extends Bundle {
  val xret = Output(Bool()) // Privilege return
  val trigger = Output(Bool()) // Exception trigger
  // Exception information
  val exceptionPC = Output(DataType.address)
  val exceptionCode = Output(DataType.exceptionCode)
  val exceptionVal = Output(DataType.operation)
  val handlerPC = Input(DataType.address) // Handler PC
}
