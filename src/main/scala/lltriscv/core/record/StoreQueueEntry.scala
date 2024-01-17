package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.execute.MemoryAccessLength

class StoreQueueAllocIO extends Bundle {
  val writeType = Output(MemoryAccessLength())
  val address = Output(DataType.address)
  val data = Output(DataType.operation)
  val id = Input(DataType.receipt)
  val valid = Output(Bool())
  val ready = Input(Bool())
}

class StoreQueueDequeueEntry extends Bundle {
  val writeType = MemoryAccessLength()
  val address = DataType.address
  val data = DataType.operation
  val valid = Bool()
}

class StoreQueueRetireIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val id = DataType.receipt
        val en = Bool()
      }
    )
  )
}

class StoreQueueEntry extends Bundle {
  val writeType = MemoryAccessLength()
  val address = DataType.address
  val data = DataType.operation
  val retire = Bool()
  val valid = Bool()
}
