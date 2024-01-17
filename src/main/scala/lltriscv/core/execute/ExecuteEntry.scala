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
  val rs1 = new DataBroadcastSlotEntry() // rs1
  val rs2 = new DataBroadcastSlotEntry() // rs2
  val rd = DataType.receipt // rd
  val func3 = DataType.func3 // func3
  val func7 = DataType.func7 // func7
  val imm = DataType.immediate // Immediate
  val zimm = DataType.zimmediate // CSR zimm
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Instruction exception code
  *
  * RISC-V instruction exception code
  */
object ExceptionCode {
  val instructionAddressMisaligned = 0.U
  val instructionAccessFault = 1.U
  val illegalInstruction = 2.U
  val breakpoint = 3.U
  val loadAddressMisaligned = 4.U
  val loadAccessFault = 5.U
  val storeAMOAddressMisaligned = 6.U
  val storeAMOAccessFault = 7.U
  val environmentCallFromUMode = 8.U
  val environmentCallFromSMode = 9.U
  // 10 Reserved
  val environmentCallFromMMode = 11.U
  val instructionPageFault = 12.U
  val loadPageFault = 13.U
  // 14 Reserved
  val storeAMOPageFault = 15.U
}

/** Execute result entry
  *
  * The output of execute component
  */
class ExecuteResultEntry extends Bundle {
  // General execution result field, writeback to rd
  val result = DataType.operation

  // Exception field
  val exception = Bool()
  val exceptionCode = DataType.exceptionCode

  // Write memory field
  val write = Bool()
  val writeID = DataType.receipt

  // CSR field
  val writeCSR = Bool()
  val csrAddress = DataType.csr
  val csrData = DataType.operation

  // XReturn field
  val xret = Bool()

  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val real = DataType.address // Real PC
  val valid = Bool() // Validity

  // Helper functions
  def noException() = {
    exception := false.B
    exceptionCode := 0.U
  }

  def triggerException(code: UInt) = {
    exception := true.B
    exceptionCode := code
  }

  def noCSR() = {
    writeCSR := false.B
    csrAddress := 0.U
    csrData := 0.U
  }

  def resultCSR(addr: UInt, wdata: UInt) = {
    writeCSR := true.B
    csrAddress := addr
    csrData := wdata
  }

  def noMemory() = {
    write := false.B
    writeID := 0.U
  }

  def resultMemory(id: UInt) = {
    write := true.B
    writeID := id
  }

  def noResult() = {
    result := 0.U
    noException()
    noCSR()
    noMemory()
    xret := false.B
    rd := 0.U
    pc := 0.U
    next := 0.U
    real := 0.U
    valid := false.B
  }
}

/** Execute queue enqueue interface
  */
class ExecuteQueueEnqueueIO extends Bundle {
  val enq = DecoupledIO(new ExecuteEntry()) // Data interface
  val queueType = Input(ExecuteQueueType()) // Tell the issue stage the queue type, hardwired
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
  val none, memory, alu, branch = Value
}
