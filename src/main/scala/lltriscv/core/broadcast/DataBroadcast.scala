package lltriscv.core.broadcast

import chisel3._
import chisel3.util._

import lltriscv.core._

class DataBroadcastSlotEntry extends Bundle {
  val pending = Bool()
  val receipt = DataType.receiptType.cloneType
}

class DataBroadcastEntry extends Bundle {
  val valid = Bool()
  val receipt = DataType.receiptType.cloneType
  val data = DataType.operationType.cloneType
}

class DataBroadcastIO extends Bundle {
  val entries = Output(Vec(2, new DataBroadcastEntry()))
}
