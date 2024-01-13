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

/** Execute entry
  *
  * The entry of reservation station, representing an executable instruction
  */
class ExecuteEntry extends Bundle {
  val opcode = DataType.opcode // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = new DataBroadcastSlotEntry() // rs1
  val rs2 = new DataBroadcastSlotEntry() // rs2
  val rd = DataType.receipt // rd
  val func3 = DataType.func3 // func3
  val func7 = DataType.func7 // func7
  val imm = DataType.immediate // Immediate
  val pc = DataType.pc // Corresponding PC
  val next = DataType.pc // Next PC
  val valid = Bool() // Validity
}

/** Execute result entry
  *
  * The output of execute component
  */
class ExecuteResultEntry extends Bundle {
  val result =
    DataType.operation // General execution result, writeback to rd
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.pc // Corresponding PC
  val next = DataType.pc // Next PC
  val real = DataType.pc // Real PC
  val valid = Bool() // Validity
}

/** ExecuteQueueEnqueueIO
  *
  * Execute queue enqueue interface
  */
class ExecuteQueueEnqueueIO extends Bundle {
  val enq = DecoupledIO(new ExecuteEntry()) // Data interface
  val queueType =
    Input(ExecuteQueueType()) // Tell the issue stage the queue type, hardwired
}

/** Execute queue type
  */
object ExecuteQueueType extends ChiselEnum {
  /*
   * none: Discarded instructions
   * memory: Memory access instructions
   * alu: ALU instructions
   * alu2: ALU instructions, only for test
   * branch: Branch instructions
   */
  val none, memory, alu, alu2, branch = Value
}
