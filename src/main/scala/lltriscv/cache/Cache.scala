package lltriscv.cache

import chisel3._
import chisel3.util._

import lltriscv.core.DataType
import lltriscv.core.CoreConstant
import lltriscv.core.execute.MemoryAccessLength

import lltriscv.bus.SMAReaderIO
import lltriscv.bus.SMAWriterIO

import lltriscv.utils.CoreUtils._
import lltriscv.utils.ChiselUtils._
import lltriscv.utils.ReadWriteSRAM

/*
 * Cache
 *
 * Default cache implementations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Cache line request to SMA
  *
  * Conversion of cache line requests to SMA interface with 32-bit bandwidth.
  *
  * @param cacheLineDepth
  *   Require 32-bit aligned cache line depth
  */
class CacheLineRequest2SMA(cacheLineDepth: Int) extends Module {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Require 32-bit aligned cache line depth")

  val io = IO(new Bundle {
    val request = Flipped(new CacheLineRequestIO(cacheLineDepth))
    val smaReader = new SMAReaderIO()
  })

  private object Status extends ChiselEnum {
    val idle, working, finish = Value
  }

  private val statusReg = RegInit(Status.idle)
  private val errorReg = RegInit(false.B)

  // counterReg: Current cell pointer
  private val incr = WireInit(false.B)
  private val (counterReg, nextValue) = pointer(cacheLineDepth / 2, incr)

  // Cache line buffer
  private val dataReg = RegInit(Vec(cacheLineDepth, UInt(CoreConstant.cacheCellLength.W)).zero)

  when(statusReg === Status.idle && io.request.valid) {
    statusReg := Status.working
    errorReg := false.B
  }

  io.smaReader.valid := false.B
  io.smaReader.readType := MemoryAccessLength.word
  io.smaReader.address := io.request.address(31, log2Ceil(cacheLineDepth) + 1) ## counterReg ## 0.U(2.W)

  when(statusReg === Status.working) {
    io.smaReader.valid := true.B

    when(io.smaReader.ready) {
      incr := true.B
      dataReg(counterReg ## 1.U) := io.smaReader.data(31, 16)
      dataReg(counterReg ## 0.U) := io.smaReader.data(15, 0)
      errorReg := errorReg || io.smaReader.error

      when(counterReg === (cacheLineDepth / 2 - 1).U) { // Finish
        statusReg := Status.finish
      }
    }
  }

  io.request.ready := false.B
  io.request.error := errorReg
  io.request.data := dataReg

  when(statusReg === Status.finish) {
    io.request.ready := true.B
    statusReg := Status.idle
  }
}

/** SMA to cache line request
  *
  * Conversion of SMA interface to cache line requests
  *
  * @param cacheLineDepth
  *   Require 32-bit aligned cache line depth
  */
class SMA2CacheLineRequest(cacheLineDepth: Int) extends Module {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Require 32-bit aligned cache line depth")

  val io = IO(new Bundle {
    val request = new CacheLineRequestIO(cacheLineDepth)
    val smaReader = Flipped(new SMAReaderIO())
  })

  io.request.valid := io.smaReader.valid
  io.request.address := getCacheLineAddress(io.smaReader.address, cacheLineDepth)

  io.smaReader.ready := io.request.ready
  io.smaReader.error := io.request.error
  io.smaReader.data := 0.U
  switch(io.smaReader.readType) {
    is(MemoryAccessLength.byte) {
      io.smaReader.data := Mux(
        io.smaReader.address(0),
        io.request.data(getCacheLineOffset(io.smaReader.address, cacheLineDepth))(15, 8),
        io.request.data(getCacheLineOffset(io.smaReader.address, cacheLineDepth))(7, 0)
      )
    }
    is(MemoryAccessLength.half) {
      io.smaReader.data := io.request.data(getCacheLineOffset(io.smaReader.address, cacheLineDepth))
    }
    is(MemoryAccessLength.word) {
      io.smaReader.data := io.request.data(getCacheLineOffset(io.smaReader.address, cacheLineDepth) + 1.U) ## io.request.data(getCacheLineOffset(io.smaReader.address, cacheLineDepth))
    }
  }
}

/** Set associative cache (Write-Through)
  *
  * This implementation uses a set associative strategy and write-through(without allocation).
  *
  * Using random replacement algorithm
  *
  * @param tagDepth
  *   Tag depth
  * @param wayDepth
  *   Way depth
  * @param cacheLineDepth
  *   Require 32-bit aligned cache line depth
  */
class SetCache(tagDepth: Int, wayDepth: Int, cacheLineDepth: Int) extends Module {
  require(tagDepth > 0 && tagDepth % 2 == 0, "Insufficient tags")
  require(wayDepth > 0, "Insufficient ways")
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Require 32-bit aligned cache line depth")

  val io = IO(new Bundle {
    // In
    val inWriter = Flipped(new SMAWriterIO())
    val inReader = Flipped(new CacheLineRequestIO(cacheLineDepth))

    // Out
    val outWriter = new SMAWriterIO()
    val outReader = new CacheLineRequestIO(cacheLineDepth)

    // Flush: invalidate all entries
    val flush = Flipped(new FlushCacheIO())
  })

  private val tagMem = Seq.fill(wayDepth)(Module(new ReadWriteSRAM(DataType.address)(tagDepth)))
  private val dataMem = Seq.fill(wayDepth)(Module(new ReadWriteSRAM(Vec(cacheLineDepth, UInt(16.W)))(tagDepth)))

  private val victimIncr = WireInit(false.B)
  private val (victimReg, _) = pointer(wayDepth, victimIncr)

  private object Status extends ChiselEnum {
    val idle, lookup, read, write = Value;
  }

  private val statusReg = RegInit(Status.idle)
  private val responseReadReg = RegInit(false.B)

  // Init
  io.inWriter <> new SMAWriterIO().zero
  io.inReader <> new CacheLineRequestIO(cacheLineDepth).zero
  io.outWriter <> new SMAWriterIO().zero
  io.outReader <> new CacheLineRequestIO(cacheLineDepth).zero
  io.flush <> new FlushCacheIO().zero

  tagMem.foreach { item =>
    item.io.dataIn := 0.U
    item.io.addr := 0.U
    item.io.write := false.B
  }
  dataMem.foreach { item =>
    item.io.dataIn := Vec(cacheLineDepth, UInt(16.W)).zero
    item.io.addr := 0.U
    item.io.write := false.B
  }

  when(statusReg === Status.idle) {
    val address = Mux(responseReadReg, io.inReader.address, io.inWriter.address)

    tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
      val tag = getCacheLineTag(address, tagDepth)
      tagWay.io.addr := tag
      dataWay.io.addr := tag
    }

    when(io.flush.req) {
      // validRegs.foreach(_.foreach(_ := false.B))
      // TODO
      io.flush.empty := true.B
    }.elsewhen(io.inReader.valid) {
      responseReadReg := true.B
      statusReg := Status.lookup
    }.elsewhen(io.inWriter.valid) {
      responseReadReg := false.B
      statusReg := Status.lookup
    }
  }

  when(statusReg === Status.lookup) {
    val address = Mux(responseReadReg, io.inReader.address, io.inWriter.address)
    val tag = getCacheLineTag(address, tagDepth)
    val offset = getCacheLineOffset(address, cacheLineDepth)
    val hit = WireInit(false.B)

    tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
      tagWay.io.addr := tag
      dataWay.io.addr := tag

      val tagCell = tagWay.io.dataOut
      val dataCell = dataWay.io.dataOut

      when(tagCell(0) && tagCell(31, 1) === getCacheLineAddress(address, cacheLineDepth)) { // Hit
        hit := true.B
        when(responseReadReg) { // Hit read
          io.inReader.error := false.B
          io.inReader.data := dataCell
          io.inReader.ready := true.B

          statusReg := Status.idle
        }.otherwise { // Hit write
          val newData = WireInit(dataCell)
          switch(io.inWriter.writeType) {
            is(MemoryAccessLength.byte) {
              newData(offset) := Mux(address(0), io.inWriter.data(7, 0) ## dataCell(offset)(7, 0), dataCell(offset)(15, 8) ## io.inWriter.data(7, 0))
            }
            is(MemoryAccessLength.half) {
              newData(offset) := io.inWriter.data(15, 0)
            }
            is(MemoryAccessLength.word) {
              newData(offset) := io.inWriter.data(15, 0)
              newData(offset + 1.U) := io.inWriter.data(31, 16)
            }
          }
          dataWay.io.dataIn := newData
          dataWay.io.write := true.B
        }
      }
    }

    when(responseReadReg && !hit) { // Miss
      statusReg := Status.read
    }.elsewhen(!responseReadReg) { // Write-Through
      statusReg := Status.write
    }
  }

  when(statusReg === Status.read) {
    val address = io.inReader.address
    val tag = getCacheLineTag(address, tagDepth)

    io.outReader.address := address
    io.outReader.valid := true.B

    when(io.outReader.ready) {
      tagMem.zip(dataMem).zipWithIndex.foreach { case ((tagWay, dataWay), way) =>
        when(way.U === victimReg) {
          tagWay.io.addr := tag
          tagWay.io.dataIn := getCacheLineAddress(address, cacheLineDepth) ## 1.U
          tagWay.io.write := !io.outReader.error // Do not write when an error occurs

          dataWay.io.addr := tag
          dataWay.io.dataIn := io.outReader.data
          dataWay.io.write := !io.outReader.error
        }
      }
      victimIncr := true.B

      io.inReader.error := io.outReader.error
      io.inReader.data := io.outReader.data
      io.inReader.ready := true.B
      statusReg := Status.idle
    }
  }

  when(statusReg === Status.write) {
    io.inWriter <> io.outWriter
    when(io.outWriter.ready) {
      statusReg := Status.idle
    }
  }
}
