package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._

object BranchOperationType extends ChiselEnum {
  val none, undefined, eq, ne, lt, ge, ltu, geu, jal = Value
}

class BranchExecuteStageEntry extends Bundle {
  val op = BranchOperationType() // Branch operation type
  val op1 = DataType.operationType.cloneType // Operand 1
  val op2 = DataType.operationType.cloneType // Operand 2
  val add1 = DataType.operationType.cloneType // Address addend 1
  val add2 = DataType.operationType.cloneType // Address addend 2
  val rd = DataType.receiptType.cloneType // Destination receipt
  val pc = DataType.pcType.cloneType // Corresponding PC
  val next = DataType.pcType.cloneType // Next PC
  val valid = Bool() // Validity
}
