package bergamot.test.mock

import chisel3._
import chisel3.util._
import bergamot.bus.SMAReaderIO
import chiseltest._
import bergamot.utils.ChiselUtils
import java.io.File
import java.io.FileInputStream
import bergamot.bus.SMAWriterIO
import bergamot.core.execute.MemoryAccessLength

trait MemoryMock {
  def loadByte(addr: Int): Byte
  def loadShort(addr: Int): Short =
    (((loadByte(addr + 1).toInt << 8) & 0xff00) | (loadByte(addr).toInt & 0x00ff)).toShort
  def loadInt(addr: Int): Int =
    ((loadByte(addr + 3).toInt << 24) & 0xff000000) |
      ((loadByte(addr + 2).toInt << 16) & 0x00ff0000) |
      ((loadByte(addr + 1).toInt << 8) & 0x0000ff00) |
      (loadByte(addr).toInt & 0x000000ff)

  def storeByte(addr: Int, value: Byte): Unit
  def storeShort(addr: Int, value: Short): Unit = {
    storeByte(addr, (value & 0xff).toByte)
    storeByte(addr + 1, ((value >> 8) & 0xff).toByte)
  }
  def storeInt(addr: Int, value: Int): Unit = {
    storeByte(addr, (value & 0xff).toByte)
    storeByte(addr + 1, ((value >> 8) & 0xff).toByte)
    storeByte(addr + 2, ((value >> 16) & 0xff).toByte)
    storeByte(addr + 3, ((value >> 24) & 0xff).toByte)
  }
}

trait MemoryFileMock extends MemoryMock {
  def importBin(file: File, start: Int) = {
    val in = new FileInputStream(file)
    var by = in.read()
    var id = 0
    while (by != -1) {
      storeByte(start + id, by.toByte)
      by = in.read()
      id = id + 1
    }
    in.close()
    println(s"Import from ${file.getName()} ${id} bytes.")
  }
}

trait SMAMemoryMock extends MemoryMock {
  def doMemory(reader: SMAReaderIO, writer: SMAWriterIO) = {
    reader.ready.poke(false.B)
    writer.ready.poke(false.B)
    if (reader.valid.peekBoolean()) {
      reader.ready.poke(true.B)
      if (reader.readType.peek() == MemoryAccessLength.byte) {
        reader.data.poke(ChiselUtils.int2UInt(loadByte(reader.address.peekInt().toInt)))
      }

      if (reader.readType.peek() == MemoryAccessLength.half) {
        reader.data.poke(ChiselUtils.int2UInt(loadShort(reader.address.peekInt().toInt)))
      }

      if (reader.readType.peek() == MemoryAccessLength.word) {
        // val addr = reader.address.peekInt().toInt
        // println(s"Reader addr: ${addr}, data: ${memory.loadInt(addr)}")
        reader.data.poke(ChiselUtils.int2UInt(loadInt(reader.address.peekInt().toInt)))
      }
    } else if (writer.valid.peekBoolean()) {
      writer.ready.poke(true.B)

      if (writer.writeType.peek() == MemoryAccessLength.byte) {
        storeByte(writer.address.peekInt().toInt, ChiselUtils.BigInt2Int(writer.data.peekInt()).toByte)
      }

      if (writer.writeType.peek() == MemoryAccessLength.half) {
        storeShort(writer.address.peekInt().toInt, ChiselUtils.BigInt2Int(writer.data.peekInt()).toShort)
      }

      if (writer.writeType.peek() == MemoryAccessLength.word) {
        val addr = writer.address.peekInt().toInt
        println(s"Writer addr: ${addr}, data: ${writer.data.peekInt()}")
        // if (writer.address.peekInt().toInt == 12412) {
        //   run = false
        //   println(s"Writer addr: ${addr}, data: ${writer.data.peekInt()}")
        // }
        // if (writer.address.peekInt().toInt == 124) { run = false }
        storeInt(writer.address.peekInt().toInt, ChiselUtils.BigInt2Int(writer.data.peekInt()))
      }
    }
  }
}

class MapMemoryMock(items: (Int, Byte)*) extends MemoryMock {
  private val memory = scala.collection.mutable.Map(items: _*)
  def loadByte(addr: Int): Byte = memory(addr)
  def storeByte(addr: Int, value: Byte): Unit = memory(addr) = value
}

class FlatMemoryMock(size: Int) extends MemoryMock {
  private val memory = Array.ofDim[Byte](size)
  def loadByte(addr: Int): Byte = memory(addr)
  def storeByte(addr: Int, value: Byte): Unit = memory(addr) = value
}

object MemoryMocks {
  def smaMock(reader: SMAReaderIO, clock: Clock, memory: MemoryMock) = {
    if (reader.valid.peekBoolean()) {
      reader.ready.poke(true.B)
      reader.error.poke(false.B)
      val address = reader.address.peekInt().toInt
      reader.data.poke(ChiselUtils.int2UInt(memory.loadInt(address)))
    } else {
      reader.ready.poke(false.B)
    }
  }
}
