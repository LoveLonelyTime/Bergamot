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
  val receipt = DataType.receiptType.cloneType
}

/** Data broadcast entry
  *
  * A data broadcasts
  */
class DataBroadcastEntry extends Bundle {
  val valid = Bool()
  // Broadcast receipt
  val receipt = DataType.receiptType.cloneType
  // Data
  val data = DataType.operationType.cloneType
}

/** Data broadcast interface
  */
class DataBroadcastIO extends Bundle {
  val entries = Output(Vec(2, new DataBroadcastEntry()))
}
