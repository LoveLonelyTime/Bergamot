package lltriscv.core

import chisel3._

/*
 * Data type declarations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Basic data type.
  */
object DataType {
  def address = UInt(32.W) // 32-bits address
  def opcode = UInt(7.W) // 7-bits opcode
  def instruction = UInt(32.W) // 32-bits instruction
  def register = UInt(5.W) // 5-bits register id
  def func3 = UInt(3.W) // 3-bits func3
  def func7 = UInt(7.W) // 7-bits func7
  def immediate = UInt(32.W) // 32-bits raw immediate
  def zimmediate = UInt(5.W) // 5-bits CSR zero-extend immediate
  def receipt = UInt(32.W) // 32-bits broadcast receipt
  def operation = UInt(32.W) // 32-bits operand
  def csr = UInt(12.W) // 12-bits CSR address
  def exceptionCode = UInt(5.W) // 4-bits exception code
  def strobe = UInt(4.W) // 4-bits for 32-bits(4-lanes) strobe
  def asid = UInt(9.W) // 9-bits address space ID
  def aByte = UInt(8.W) // A Byte
  def vpn = UInt(20.W) // Virtual page number
  def ppn20 = UInt(20.W) // 20-bits physical page number
  def ppn22 = UInt(22.W) // 22-bits physical page number
  def half = UInt(16.W) // half word
}

object CoreConstant {
  val XLEN = 32
  val instructionLength = 4
  val compressInstructionLength = 2
}
