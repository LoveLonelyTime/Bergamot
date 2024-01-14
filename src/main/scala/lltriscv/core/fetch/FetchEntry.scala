package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry

/*
 * Decode entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** PC verify stage entry
  *
  * The input of PCVerifyStage
  */
class PCVerifyStageEntry extends Bundle {
  val instruction = DataType.instruction // Instruction
  val pc = DataType.pc // Corresponding PC
  val spec = DataType.pc // Speculative PC
  val next = DataType.pc // Next PC
  val valid = Bool() // Validity
}
