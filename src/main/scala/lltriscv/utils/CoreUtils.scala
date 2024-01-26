package lltriscv.utils

import chisel3._
import chisel3.util._
import lltriscv.core.broadcast.DataBroadcastSlotEntry
import lltriscv.core.broadcast.DataBroadcastEntry

/*
 * Core utils collection
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

object CoreUtils {

  /** Sign extended
    *
    * @param x
    *   UInt
    * @param b
    *   Sign bit
    * @return
    *   width = x.getWidth
    */
  def signExtended(x: UInt, b: Int) = Fill(x.getWidth - b - 1, x(b)) ## x(b, 0)

  /** Create a pointer from 0 to depth - 1
    * @param depth
    *   Max value = depth - 1
    * @param incr
    *   Increse flag
    * @param init
    *   Init value
    * @return
    *   (current value, next value)
    */
  def pointer(depth: Int, incr: Bool, init: Int = 0) = {
    val cntReg = RegInit(init.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
    when(incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  /** Match a broadcast slot
    *
    * When the match is successful, reset slot pending and replace receipt
    *
    * @param slot
    *   Broadcast slot
    * @param source
    *   Broadcast source
    * @return
    */
  def matchBroadcast(
      output: DataBroadcastSlotEntry,
      slot: DataBroadcastSlotEntry,
      source: DataBroadcastEntry
  ) = {
    when(
      source.valid &&
        slot.pending &&
        slot.receipt === source.receipt
    ) {
      // Reset and replace
      output.pending := false.B
      output.receipt := source.data
    }
  }

  def Vec2[T <: Data](data: T) = Vec(2, data)
  def VecInit2[T <: Data](data: T) = VecInit.fill(2)(data)

  def isCompressInstruction(instruction: UInt) = instruction(1, 0) =/= "b11".U
}

object Sv32 {
  def getVPN(vaddr: UInt) = vaddr(31, 12)
  def getOffset(vaddr: UInt) = vaddr(11, 0)
  def get32PPN(paddr: UInt) = paddr(31, 12)
}
