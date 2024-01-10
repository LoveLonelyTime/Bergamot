package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.broadcast.DataBroadcastSlotEntry

class ExecuteEntry extends Bundle {
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

class ExecuteQueueEntry extends Bundle {
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
