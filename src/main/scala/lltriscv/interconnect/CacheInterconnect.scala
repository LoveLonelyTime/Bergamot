package lltriscv.interconnect

import chisel3._
import chisel3.util._

import lltriscv.cache.CacheLineRequestIO

import lltriscv.utils.CoreUtils._
import lltriscv.utils.ChiselUtils._

/*
 * Cache interconnect
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Cache line 2-requestors interconnect
  *
  * Priority arbitration, in1 > in2
  *
  * @param cacheLineDepth
  *   Cache line depth
  */
class CacheLineRequest2Interconnect(cacheLineDepth: Int) extends Module {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Require 32-bit aligned cache line depth")

  val io = IO(new Bundle {
    val in1 = Flipped(new CacheLineRequestIO(cacheLineDepth))
    val in2 = Flipped(new CacheLineRequestIO(cacheLineDepth))
    val out = new CacheLineRequestIO(cacheLineDepth)
  })

  private object Status extends ChiselEnum {
    val idle, pending1, pending2 = Value
  }
  private val statusReg = RegInit(Status.idle)

  io.in1 <> new CacheLineRequestIO(cacheLineDepth).zero
  io.in2 <> new CacheLineRequestIO(cacheLineDepth).zero
  io.out <> new CacheLineRequestIO(cacheLineDepth).zero

  when(statusReg === Status.idle) {
    when(io.in1.valid) {
      statusReg := Status.pending1
    }.elsewhen(io.in2.valid) {
      statusReg := Status.pending2
    }
  }

  when(statusReg === Status.pending1) {
    io.out <> io.in1

    when(io.out.ready) {
      statusReg := Status.idle
    }
  }

  when(statusReg === Status.pending2) {
    io.out <> io.in2

    when(io.out.ready) {
      statusReg := Status.idle
    }
  }
}
