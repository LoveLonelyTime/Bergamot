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
    * @return
    *   (current value, next value)
    */
  def pointer(depth: Int, incr: Bool) = {
    val cntReg = RegInit(0.U(log2Ceil(depth).W))
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
      slot: DataBroadcastSlotEntry,
      source: DataBroadcastEntry
  ) = {
    when(
      source.valid &&
        slot.pending &&
        slot.receipt === source.receipt
    ) {
      // Reset and replace
      slot.pending := false.B
      slot.receipt := source.data
    }
  }

  /** Bypass a broadcast slot
    *
    * When the match is successful, return bypass result
    *
    * Otherwise, return slot
    *
    * @param slot
    *   Broadcast slot
    * @param source
    *   Broadcast source
    * @return
    *   Bypass result
    */
  def bypassBroadcast(
      slot: DataBroadcastSlotEntry,
      source: DataBroadcastEntry
  ) = {
    val result = Wire(new DataBroadcastSlotEntry())
    when(
      source.valid &&
        slot.pending &&
        slot.receipt === source.receipt
    ) {
      result.pending := false.B
      result.receipt := source.data
    }.otherwise {
      result.pending := slot.pending
      result.receipt := slot.receipt
    }
    result
  }
}
