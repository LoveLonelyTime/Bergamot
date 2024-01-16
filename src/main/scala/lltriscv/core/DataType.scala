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
}
