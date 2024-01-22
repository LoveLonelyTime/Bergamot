package lltriscv.cache

import chisel3._
import chisel3.util._

import lltriscv.bus.SMAReaderIO
import lltriscv.bus.SMAWriterIO
import lltriscv.core.execute.MemoryAccessLength
import lltriscv.utils.CoreUtils
import lltriscv.utils.ChiselUtils._

/*
 * Cache
 *
 * Provide default cache implementations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

//! TestCode
class TrivialDCache extends Module {
  val io = IO(new Bundle {
    val upReader = Flipped(new SMAReaderIO())
    val upWriter = Flipped(new SMAWriterIO())

    val downReader = new SMAReaderIO()
    val downWriter = new SMAWriterIO()

    val flush = Flipped(new FlushCacheIO())
  })
  io.upReader <> io.downReader
  io.upWriter <> io.downWriter

  io.flush.empty := true.B
}

//! TestCode
class TrivialICache(depth: Int) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(new ICacheLineRequestIO(depth))
    val downReader = new SMAReaderIO()

    val flush = Flipped(new FlushCacheIO())
  })
  private object Status extends ChiselEnum {
    val idle, working, finish = Value
  }
  private val statusReg = RegInit(Status.idle)
  private val incr = WireInit(false.B)
  private val (counterReg, nextValue) = CoreUtils.pointer(depth, incr)
  private val dataReg = RegInit(Vec(depth, UInt(16.W)).zero)

  when(statusReg === Status.idle) {
    when(io.request.valid) {
      statusReg := Status.working
    }
  }

  io.downReader.valid := false.B
  io.downReader.readType := MemoryAccessLength.half
  io.downReader.address := io.request.address(31, log2Ceil(depth) + 1) ## counterReg ## 0.U

  when(statusReg === Status.working) {
    io.downReader.valid := true.B

    when(io.downReader.ready) {
      incr := true.B
      dataReg(counterReg) := io.downReader.data

      when(counterReg === (depth - 1).U) {
        statusReg := Status.finish
      }
    }
  }

  io.request.ready := false.B
  io.request.error := false.B
  io.request.data := dataReg

  when(statusReg === Status.finish) {
    io.request.ready := true.B

    when(io.request.valid && io.request.ready) {
      statusReg := Status.idle
    }
  }

  io.flush.empty := true.B
}
