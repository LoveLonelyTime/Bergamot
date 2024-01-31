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

/** Cache line request to SMA
  *
  * Conversion of cache line requests to SMA interface with 32-bit bandwidth.
  * @param cacheLineDepth
  *   Require 32-bit aligned cache line depth
  */
class CacheLineRequest2SMA(cacheLineDepth: Int) {
  val io = IO(new Bundle {
    val request = Flipped(new CacheLineRequestIO(cacheLineDepth))
    val smaReader = new SMAReaderIO()
  })

  private object Status extends ChiselEnum {
    val idle, working, finish = Value
  }

  private val statusReg = RegInit(Status.idle)
  private val incr = WireInit(false.B)
  private val (counterReg, nextValue) = CoreUtils.pointer(cacheLineDepth / 2, incr)
  private val dataReg = RegInit(Vec(cacheLineDepth, UInt(16.W)).zero)

  when(statusReg === Status.idle) {
    when(io.request.valid) {
      statusReg := Status.working
    }
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

      when(counterReg === (cacheLineDepth / 2 - 1).U) {
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
}

/** Set Cache (Write-Through)
  *
  * @param cacheLineDepth
  */
class SetCache(tagDepth: Int, wayDepth: Int, cacheLineDepth: Int) {
  val io = IO(new Bundle {
    // In
    val inWriter = Flipped(new SMAWriterIO())
    val inReader = Flipped(new CacheLineRequestIO(cacheLineDepth))

    // Out
    val outWriter = new SMAWriterIO()
    val outReader = new CacheLineRequestIO(cacheLineDepth)
  })

  private class TagEntry extends Bundle {
    val address = UInt(31.W)
    val valid = Bool()
  }

  private val tagMem = Seq.fill(wayDepth)(SyncReadMem(log2Ceil(tagDepth), new TagEntry()))
  private val dataMem = Seq.fill(wayDepth)(SyncReadMem(log2Ceil(tagDepth), Vec(cacheLineDepth, UInt(16.W))))

  private val victimIncr = WireInit(false.B)
  private val (victimReg, _) = CoreUtils.pointer(wayDepth, victimIncr)

  private object Status extends ChiselEnum {
    val idle, lookup, read, write = Value;
  }
  private val statusReg = RegInit(Status.idle)
  private val responseReadReg = RegInit(false.B)

  private def getTag(address: UInt) = address(31, 32 - log2Ceil(tagDepth))
  private def getCacheAddress(address: UInt) = address(31, log2Ceil(cacheLineDepth) + 1)

  when(statusReg === Status.idle) {
    val address = Mux(responseReadReg, io.inReader.address, io.inWriter.address)
    tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
      tagWay.read(getTag(address))
      dataWay.read(getTag(address))
    }

    when(io.inReader.valid) {
      responseReadReg := true.B
      statusReg := Status.lookup
    }.elsewhen(io.inWriter.valid) {
      responseReadReg := false.B
      statusReg := Status.lookup
    }
  }

  when(statusReg === Status.lookup) {
    val address = Mux(responseReadReg, io.inReader.address, io.inWriter.address)
    val hit = WireInit(false.B)

    tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
      val tagCell = tagWay.read(getTag(address))
      val dataCell = dataWay.read(getTag(address))
      when(tagCell.valid && tagCell.address === getCacheAddress(address)) { // Hit
        hit := true.B
        when(responseReadReg) { // Hit read
          io.inReader.error := false.B
          io.inReader.data := dataCell
          io.inReader.ready := true.B

          statusReg := Status.idle
        }.otherwise { // Hit write
          val newData = Wire(Vec(cacheLineDepth, UInt(16.W)))
          // TODO: Write
          dataWay.write(getTag(address), newData)
        }
      }
    }

    when(responseReadReg && !hit) {
      statusReg := Status.read
    }.elsewhen(!responseReadReg) {
      statusReg := Status.write
    }
  }

  when(statusReg === Status.read) {
    val address = io.inReader.address
    io.outReader.address := getCacheAddress(address)
    io.outReader.valid := true.B

    when(io.outReader.ready) {
      tagMem.zip(dataMem).zipWithIndex.foreach { case ((tagWay, dataWay), way) =>
        when(way.U === victimReg) {
          val entry = Wire(new TagEntry())
          entry.address := getCacheAddress(address)
          entry.valid := true.B
          tagWay.write(getTag(address), entry)
          dataWay.write(getTag(address), io.outReader.data)
        }
      }
      victimIncr := true.B

      io.inReader.error := false.B
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
