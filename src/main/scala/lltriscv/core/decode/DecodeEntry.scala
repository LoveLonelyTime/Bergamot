package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._

class DecodeStageEntry extends Bundle {
  // Instruction
  val instruction = DataType.instructionType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}

class RegisterMappingStageEntry extends Bundle {
  // opcode
  val opcode = DataType.opcodeType.cloneType
  // Instruction Type
  val instructionType = InstructionType()
  // Execute queue
  val executeQueue = ExecuteQueueType()
  // rs1
  val rs1 = DataType.registerType.cloneType
  // rs2
  val rs2 = DataType.registerType.cloneType
  // rd
  val rd = DataType.registerType.cloneType
  // func3
  val func3 = DataType.func3Type.cloneType
  // func7
  val func7 = DataType.func7Type.cloneType
  // Immediate
  val imm = DataType.immediateType.cloneType

  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}

class IssueStageEntry extends Bundle {
  // opcode
  val opcode = DataType.opcodeType.cloneType
  // Instruction Type
  val instructionType = InstructionType()
  // Execute queue
  val executeQueue = ExecuteQueueType()
  // rs1
  val rs1 = new DataBroadcastSlotEntry()
  // rs2
  val rs2 = new DataBroadcastSlotEntry()
  // rd
  val rd = DataType.receiptType.cloneType
  // func3
  val func3 = DataType.func3Type.cloneType
  // func7
  val func7 = DataType.func7Type.cloneType
  // Immediate
  val imm = DataType.immediateType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}

class ExecuteEntry extends Bundle {
  // opcode
  val opcode = DataType.opcodeType.cloneType
  // Instruction Type
  val instructionType = InstructionType()
  // rs1
  val rs1 = new DataBroadcastSlotEntry()
  // rs2
  val rs2 = new DataBroadcastSlotEntry()
  // rd
  val rd = DataType.receiptType.cloneType
  // func3
  val func3 = DataType.func3Type.cloneType
  // func7
  val func7 = DataType.func7Type.cloneType
  // Immediate
  val imm = DataType.immediateType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}
