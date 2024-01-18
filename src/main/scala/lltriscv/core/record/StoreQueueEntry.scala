package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.execute.MemoryAccessLength

/*
 * Store queue entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Store queue alloc interface
  */
class StoreQueueAllocIO extends Bundle {
  val writeType = Output(MemoryAccessLength())
  val address = Output(DataType.address)
  val data = Output(DataType.operation)
  val id = Input(DataType.receipt)
  val valid = Output(Bool())
  val ready = Input(Bool())
}

/** Store queue dequeue entry
  */
class StoreQueueDequeueEntry extends Bundle {
  val writeType = MemoryAccessLength()
  val address = DataType.address
  val data = DataType.operation
  val valid = Bool()
}

/** Store queue retire interface
  */
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

/** Store queue entry
  */
class StoreQueueEntry extends Bundle {
  val writeType = MemoryAccessLength() // Write type
  val address = DataType.address // Physical address
  val data = DataType.operation // Data
  val retire = Bool() // Has retired?
  val valid = Bool()
}
