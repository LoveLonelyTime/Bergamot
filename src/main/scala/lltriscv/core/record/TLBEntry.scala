package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

class TLBEntry extends Bundle {
  val vpn = UInt(20.W)
  val ppn = UInt(22.W)
  val r = Bool()
  val w = Bool()
  val x = Bool()
  val u = Bool()
  val g = Bool()
  val mPage = Bool()
  val error = Bool()
  val valid = Bool()
}

class TLBPAddressEntry extends Bundle {
  val address = DataType.address
  val error = Bool()
}
