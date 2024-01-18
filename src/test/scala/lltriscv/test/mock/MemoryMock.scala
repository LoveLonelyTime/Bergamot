package lltriscv.test.mock

import chisel3._
import chisel3.util._
import lltriscv.bus.SMAReaderIO
import chiseltest._
import lltriscv.utils.ChiselUtils
import java.io.File
import java.io.FileInputStream

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
