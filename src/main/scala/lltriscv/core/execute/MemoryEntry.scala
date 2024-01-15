package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._

/** Memory operation type
  */
object MemoryOperationType extends ChiselEnum {
  val none, undefined, lb, lh, lw, lbu, lhu, sb, sh, sw = Value
}

/** Memory execute stage entry
  *
  * The input entry of MemoryExecuteStage
  */
class MemoryExecuteStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val add1 = DataType.operation // Address addend 1
  val add2 = DataType.operation // Address addend 2
  val op1 = DataType.operation // Operand 1
  val op2 = DataType.operation // Operand 2
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class MemoryTLBStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val vaddress = DataType.address // Virtual address
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class MemoryReadStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val vaddress = DataType.address // Virtual address
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
