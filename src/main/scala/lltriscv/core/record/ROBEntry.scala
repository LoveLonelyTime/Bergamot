package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._

/*
 * ROB entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ROB table entry
  *
  * A ROB table entry
  */
class ROBTableEntry extends Bundle {
  // Common instruction execution result
  val result = DataType.operation
  val rd = DataType.register // Target rd
  val spec = DataType.pc // Speculative PC
  val real = DataType.pc // Real PC
  val pc = DataType.pc // Blamed PC
  val commit = Bool() // Has been committed?
  val valid = Bool() // Validity
}

/** ROB table write interface
  *
  * ROB registration interface
  */
class ROBTableWriteIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val id = DataType.receipt
        val pc = DataType.pc
        val rd = DataType.register
        val spec = DataType.pc
        val valid = Bool()
      }
    )
  ) // Registrations
  val wen = Output(Bool()) // Write enable flag
}

/** ROB table commit interface
  *
  * ROB commit interface
  */
class ROBTableCommitIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val id = DataType.receipt
        val result = DataType.operation
        val real = DataType.pc
        val valid = Bool()
      }
    )
  ) // Committing entries
  val wen = Output(Bool()) // Write enable flag
}

/** ROB table retire interface
  *
  * Used by retire component to query instructions in ROB
  *
  * @param depth
  *   The number of ROB table items, each table entry stores 2 instructions.
  */
class ROBTableRetireIO(depth: Int) extends Bundle {
  require(depth > 0, "ROB table depth must be greater than 0")

  val entries = Output(Vec(depth * 2, new ROBTableEntry()))
}
