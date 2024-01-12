package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.broadcast.DataBroadcastSlotEntry
import lltriscv.core.execute._

/*
 * Decode entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** RISC-V instruction types
  */
object InstructionType extends ChiselEnum {
  /*
   * UK: Unknown (Illegal instruction exception)
   * R: Register
   * I: Immediate
   * S: Store
   * B: Branch
   * U: Upper
   * J: Jump
   */
  val UK, R, I, S, B, U, J = Value
}

/** Decode stage entry
  *
  * The input of decode stage
  */
class DecodeStageEntry extends Bundle {
  val instruction =
    DataType.instructionType.cloneType // 32-bits raw instruction
  val pc = DataType.pcType.cloneType // Corresponding PC
  val valid = Bool() // Validity
}

/** Register mapping stage entry
  *
  * The input of register mapping stage
  */
class RegisterMappingStageEntry extends Bundle {
  val opcode = DataType.opcodeType.cloneType // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = DataType.registerType.cloneType // rs1
  val rs2 = DataType.registerType.cloneType // rs2
  val rd = DataType.registerType.cloneType // rd
  val func3 = DataType.func3Type.cloneType // func3
  val func7 = DataType.func7Type.cloneType // func7
  val imm = DataType.immediateType.cloneType // Immediate
  val pc = DataType.pcType.cloneType // Corresponding PC
  val valid = Bool() // Validity
}

/** Issue stage entry
  *
  * The input of issue stage
  */
class IssueStageEntry extends Bundle {
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
