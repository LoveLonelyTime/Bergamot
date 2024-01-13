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
  val result = DataType.operationType.cloneType
  val rd = DataType.registerType.cloneType // Target rd
  val spec = DataType.pcType.cloneType // Speculative PC
  val real = DataType.pcType.cloneType // Real PC
  val pc = DataType.pcType.cloneType // Blamed PC
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
        val id = DataType.receiptType.cloneType
        val pc = DataType.pcType.cloneType
        val rd = DataType.registerType.cloneType
        val spec = DataType.pcType.cloneType
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
        val id = DataType.receiptType.cloneType
        val result = DataType.operationType.cloneType
        val real = DataType.pcType.cloneType
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
  val entries = Output(Vec(depth * 2, new ROBTableEntry())) // Entries
}
