package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.broadcast.DataBroadcastSlotEntry

/*
 * Register mapping table entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Register mapping interface
  */
class RegisterMappingIO extends Bundle {
  // Request Registers
  val regGroup = Output(
    Vec(
      2,
      new Bundle {
        val rs1 = DataType.registerType.cloneType
        val rs2 = DataType.registerType.cloneType
        val rd = DataType.registerType.cloneType
      }
    )
  )

  // Response registers
  val mappingGroup = Input(
    Vec(
      2,
      new Bundle {
        val rs1 = new DataBroadcastSlotEntry()
        val rs2 = new DataBroadcastSlotEntry()
        val rd = DataType.receiptType.cloneType
      }
    )
  )

  // Handshake flags
  val valid = Output(Bool())
  val ready = Input(Bool())
}

/** Register mapping table entry
  *
  * A register mapping table entry
  */
class RegisterMappingTableEntry extends Bundle {
  val content = new DataBroadcastSlotEntry()

  // Register content, used to recover speculative register content
  val recover = DataType.operationType.cloneType
}
