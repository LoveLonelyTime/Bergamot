package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._

/*
 * ALU entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ALU operation type
  */
object ALUOperationType extends ChiselEnum {
  /*
   * Reserve operation:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Arithmetic operation:
   * - add: op1 + op2
   * - sub: op1 - op2
   *
   * Logical operation:
   * - and: op1 & op2
   * - or: op1 | op2
   * - xor: op1 ^ op2
   *
   * Shift operation:
   * - sll: op1 << op2
   * - srl: op1 >> op2 (zero-extend)
   * - sra: op1 >> op2 (sign-extend)
   *
   * Compare then set operation:
   * - slt: if op1 < op1 then 1 else 0
   * - sltu: if op1 < op1 (unsigned) then 1 else 0
   */
  val none, undefined, add, sub, and, or, xor, sll, srl, sra, slt, sltu = Value
}

/** ALU execute stage entry
  *
  * The input entry of ALUExecuteStage
  */
class ALUExecuteStageEntry extends Bundle {
  val op = ALUOperationType() // ALU operation type
  val op1 = DataType.operationType.cloneType // Operand 1
  val op2 = DataType.operationType.cloneType // Operand 2
  val rd = DataType.receiptType.cloneType // Destination receipt
  val pc = DataType.pcType.cloneType // Corresponding PC
  val next = DataType.pcType.cloneType // Next PC
  val valid = Bool() // Validity
}
