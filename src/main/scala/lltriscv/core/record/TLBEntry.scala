package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.execute.MemoryErrorCode

/*
 * TLB entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** TLB entry
  */
class TLBEntry extends Bundle {
  val vpn = UInt(20.W) // Virtual page number
  val ppn = UInt(22.W) // Physical page numer
  val asid = UInt(9.W) // Address space ID
  val uxwr = UInt(4.W) // User/Execute/Write/Read
  val da = UInt(2.W) // Dirty/Access
  val g = Bool() // Global
  val v = Bool() // Valid
  val mPage = Bool() // MiB page
  val valid = Bool() // Entry valid
}

/** TLB request interface
  */
class TLBRequestIO extends Bundle {
  val vaddress = Output(DataType.address) // Virtual address
  val write = Output(Bool()) // Store operation
  val paddress = Input(DataType.address) // Physical address
  val error = Input(MemoryErrorCode()) // Error
  val valid = Output(Bool())
  val ready = Input(Bool())
}
