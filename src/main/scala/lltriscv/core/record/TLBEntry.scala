package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

class TLBEntry extends Bundle {
  val vpn = UInt(20.W)
  val ppn = UInt(22.W)
  val uxwr = UInt(4.W)
  val mPage = Bool()
  val valid = Bool()
}

object TLBErrorCode extends ChiselEnum {
  /*
   * success: Translation successful
   * accessFault: Access denied
   * pageFault: Invalid page table
   */
  val success, pageFault, accessFault = Value
}

object PTERWX {
  val next = "b000".U
  val r = "b001".U
  val reservedW = "b010".U
  val rw = "b011".U
  val x = "b100".U
  val rx = "b101".U
  val reservedWX = "b110".U
  val rwx = "b111".U
}

class TLBVAddressEntry extends Bundle {
  val address = DataType.address
  val write = Bool()
}

class TLBPAddressEntry extends Bundle {
  val address = DataType.address
  val error = TLBErrorCode()
}
