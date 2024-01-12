package lltriscv.core

import chisel3._

/*
 * Data type declarations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Basic data type.
  */
object DataType {
  val pcType = UInt(32.W) // 32-bits instruction address
  val opcodeType = UInt(7.W) // 7-bits opcode
  val instructionType = UInt(32.W) // 32-bits instruction
  val registerType = UInt(5.W) // 5-bits register id
  val func3Type = UInt(3.W) // 3-bits func3
  val func7Type = UInt(7.W) // 7-bits func7
  val immediateType = UInt(32.W) // 32-bits raw immediate
  val receiptType = UInt(32.W) // 32-bits broadcast receipt
  val operationType = UInt(32.W) // 32-bits operand
}
