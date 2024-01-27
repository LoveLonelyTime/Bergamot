package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.broadcast.DataBroadcastSlotEntry

import lltriscv.utils.CoreUtils._
import lltriscv.utils.ChiselUtils._

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
    Vec2(
      new Bundle {
        val rs1 = DataType.register
        val rs2 = DataType.register
        val rs3 = DataType.register
        val rd = DataType.register
      }
    )
  )

  // Response registers
  val mappingGroup = Input(
    Vec2(
      new Bundle {
        val rs1 = new DataBroadcastSlotEntry()
        val rs2 = new DataBroadcastSlotEntry()
        val rs3 = new DataBroadcastSlotEntry()
        val rd = DataType.receipt
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
  // Speculative content
  val content = new DataBroadcastSlotEntry()

  // Register content, used to recover speculative register content
  val recover = DataType.operation
}

/** Register update interface
  *
  * When an instruction retires, update the status of the core registers through this interface
  */
class RegisterUpdateIO extends Bundle {
  val entries = Output(
    Vec2(
      new Bundle {
        val rd = DataType.register
        val result = DataType.operation
      }
    )
  )
}
