package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.fpu.FPEntry
import bergamot.core.fpu.FPMulEntry
import bergamot.core.fpu.FPAddEntry

object FPUOperationType extends ChiselEnum {
  val undefined, madd, msub, nmsub, nmadd, add, sub, mul, div, sqrt, mvWX, mvXW = Value
}

class FPOperand extends Bundle {
  val raw = DataType.operation
  val fpClass = DataType.operation
  val entry = new FPEntry()
}

class FPUMulStageEntry extends Bundle {
  val op = FPUOperationType() // ALU operation type
  val fpOp1 = new FPOperand() // FP operand 1
  val fpOp2 = new FPOperand() // FP operand 2
  val fpOp3 = new FPOperand() // FP operand 3
  val xOp = DataType.operation // Integer operand
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class FPUAddStageEntry extends Bundle {
  val op = FPUOperationType() // ALU operation type
  val fpOp1 = new FPMulEntry() // FP operand 1
  val fpOp2 = new FPMulEntry() // FP operand 2
  val xOp = DataType.operation // Integer operand
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class FPUBoxStageEntry extends Bundle {
  val op = FPUOperationType() // ALU operation type
  val fpOp1 = new FPAddEntry() // FP operand 1
  val fpOp2 = new FPAddEntry() // FP operand 2
  val raw = DataType.operation // Raw operand
  val xOp = DataType.operation // Integer operand
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
