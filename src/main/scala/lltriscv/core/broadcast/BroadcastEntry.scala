package lltriscv.core.broadcast

import chisel3._
import chisel3.util._

import lltriscv.core._

/*
 * Broadcast entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Data broadcast slot entry
  *
  * A broadcast receiving slot
  */
class DataBroadcastSlotEntry extends Bundle {
  val pending = Bool() // Has it been received?
  // When it has been received, receipt field is data received
  // Otherwise, receipt field is broadcast receipt
  val receipt = DataType.receipt
}

/** Data broadcast entry
  *
  * A data broadcasts
  */
class DataBroadcastEntry extends Bundle {
  val valid = Bool()
  // Broadcast receipt
  val receipt = DataType.receipt
  // Data
  val data = DataType.operation

  def castBroadcast(receipt: UInt, data: UInt) = {
    valid := true.B
    this.receipt := receipt
    this.data := data
  }

  def noBroadcast() = {
    valid := false.B
    this.receipt := 0.U
    this.data := 0.U
  }
}

/** Data broadcast interface
  */
class DataBroadcastIO extends Bundle {
  val entries = Output(Vec(2, new DataBroadcastEntry()))
}
