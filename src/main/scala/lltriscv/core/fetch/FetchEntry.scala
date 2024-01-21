package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry
import lltriscv.core.record.TLBErrorCode

/*
 * Decode entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

object ICacheLineWorkErrorCode extends ChiselEnum {
  val none, memoryFault, pageFault = Value
}

class ITLBWorkEntry extends Bundle {
  val vpn = UInt(20.W) // Virtual page number
  val ppn = UInt(22.W) // Physical page numer
  val error = TLBErrorCode()
  val valid = Bool()
}

class ICacheLineWorkEntry extends Bundle {
  val content = Vec(8, UInt(16.W))
  val pc = DataType.address
  val error = ICacheLineWorkErrorCode()
  val valid = Bool()
}

class RawInstructionEntry extends Bundle {
  val instruction = DataType.instruction
  val compress = Bool()
  val error = ICacheLineWorkErrorCode()
  val valid = Bool()
}

class SpeculativeEntry extends Bundle {
  val instruction = DataType.instruction // Instruction
  val pc = DataType.address // Corresponding PC
  val spec = DataType.address // Speculative PC
  val next = DataType.address // Next PC
  val error = ICacheLineWorkErrorCode()
  val valid = Bool() // Validity
}
