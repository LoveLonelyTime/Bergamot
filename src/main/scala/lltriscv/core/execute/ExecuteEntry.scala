package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.decode._
import lltriscv.core.broadcast.DataBroadcastSlotEntry

/*
 * Execute entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Execute queue entry
  *
  * The input of reservation station
  */
class ExecuteQueueEntry extends Bundle {
  val opcode = DataType.opcodeType.cloneType // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = new DataBroadcastSlotEntry() // rs1
  val rs2 = new DataBroadcastSlotEntry() // rs2
  val rd = DataType.receiptType.cloneType // rd
  val func3 = DataType.func3Type.cloneType // func3
  val func7 = DataType.func7Type.cloneType // func7
  val imm = DataType.immediateType.cloneType // Immediate
  val pc = DataType.pcType.cloneType // Corresponding PC
  val valid = Bool() // Validity
}

/** Execute entry
  *
  * The output of reservation station, representing an executable instruction
  */
class ExecuteEntry extends Bundle {
  val opcode = DataType.opcodeType.cloneType // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = new DataBroadcastSlotEntry() // rs1
  val rs2 = new DataBroadcastSlotEntry() // rs2
  val rd = DataType.receiptType.cloneType // rd
  val func3 = DataType.func3Type.cloneType // func3
  val func7 = DataType.func7Type.cloneType // func7
  val imm = DataType.immediateType.cloneType // Immediate
  val pc = DataType.pcType.cloneType // Corresponding PC
  val valid = Bool() // Validity
}

/** Execute result entry
  *
  * The output of execute component
  */
class ExecuteResultEntry extends Bundle {
  val result =
    DataType.operationType.cloneType // General execution result, writeback to rd
  val rd = DataType.receiptType.cloneType // Destination receipt
  val pc = DataType.pcType.cloneType // Corresponding PC
  val valid = Bool() // Validity
}

/** Execute queue type
  */
object ExecuteQueueType extends ChiselEnum {
  /*
   * none: Discarded instructions
   * memory: Memory access instructions
   * alu: ALU instructions
   * branch: Branch instructions
   */
  val none, memory, alu, alu2, branch = Value
}
