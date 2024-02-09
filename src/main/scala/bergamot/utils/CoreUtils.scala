package bergamot.utils

import chisel3._
import chisel3.util._
import bergamot.core.broadcast.DataBroadcastSlotEntry
import bergamot.core.broadcast.DataBroadcastEntry
import bergamot.core.CoreConstant

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

  def getWordAddress(addr: UInt) = addr(CoreConstant.XLEN - 1, 2)
  def getCacheLineTag(address: UInt, tagDepth: Int) = address(CoreConstant.XLEN - 1, CoreConstant.XLEN - log2Ceil(tagDepth))
  def getCacheLineAddress(address: UInt, cacheLineDepth: Int) = address(CoreConstant.XLEN - 1, log2Ceil(cacheLineDepth) + 1) ## 0.U((log2Ceil(cacheLineDepth) + 1).W)
  def getCacheLineOffset(address: UInt, cacheLineDepth: Int) = address(log2Ceil(cacheLineDepth), 1)
}

object Sv32 {
  def getVPN(vaddr: UInt) = vaddr(31, 12)
  def getOffset(vaddr: UInt) = vaddr(11, 0)
  def getMOffset(vaddr: UInt) = vaddr(21, 0)
  def get32PPN(paddr: UInt) = paddr(31, 12)
  def getVPN0(vaddr: UInt) = vaddr(21, 12)
  def getVPN1(vaddr: UInt) = vaddr(31, 22)
}
