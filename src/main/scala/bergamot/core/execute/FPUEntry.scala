package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.fpu.FPEntry
import bergamot.core.fpu.FPMulEntry
import bergamot.core.fpu.FPAddEntry
import bergamot.core.fpu.FPException

object FPUOperationType extends ChiselEnum {
  val undefined, madd, msub, nmsub, nmadd, add, sub, mul, div, sqrt, mvWX, mvXW = Value
}

object FPUOperationWidth extends ChiselEnum {
  val undefined, float, double = Value
}

class FPOperand extends Bundle {
  val raw = DataType.operation
  val fpClass = DataType.operation
  val entry = new FPEntry()
}

class FPUMulStageEntry extends Bundle {
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
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
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
  val fpOp1 = new FPMulEntry() // FP operand 1
  val fpOp2 = new FPMulEntry() // FP operand 2
  val fpRes = new FPAddEntry() // FP lead result
  val xOp = DataType.operation // Integer operand
  val exception = new FPException() // FP exception
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class FPUBoxStageEntry extends Bundle {
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
  val fpOp = new FPAddEntry() // FP operand
  val xOp = DataType.operation // Integer operand
  val exception = new FPException() // FP exception
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
