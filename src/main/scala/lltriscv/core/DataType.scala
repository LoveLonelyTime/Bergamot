package lltriscv.core

import chisel3._

/** Basic data type.
  */
object DataType {
  // 32-bit instruction address
  val pcType = UInt(32.W)
  // 32-bit instruction
  val opcodeType = UInt(7.W)
  val instructionType = UInt(32.W)
  val registerType = UInt(5.W)
  val func3Type = UInt(3.W)
  val func7Type = UInt(7.W)
  val immediateType = UInt(32.W)
  val receiptType = UInt(32.W)
  val operationType = UInt(32.W)
}

object InstructionType extends ChiselEnum {
  val R, I, S, B, U, J, UK = Value
}

object ExecuteQueueType extends ChiselEnum {
  val memory, alu, branch, none = Value
}
