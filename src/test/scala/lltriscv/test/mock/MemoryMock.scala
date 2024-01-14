package lltriscv.test.mock

import chisel3._
import chisel3.util._
import lltriscv.bus.SMAReaderIO
import chiseltest._
import lltriscv.utils.ChiselUtils

trait MemoryMock {
  def loadByte(addr: Int): Byte
  def loadShort(addr: Int): Short =
    ((loadByte(addr + 1).toInt << 8) | loadByte(addr).toInt).toShort
  def loadInt(addr: Int): Int =
    (loadByte(addr + 3).toInt << 24) |
      (loadByte(addr + 2).toInt << 16) |
      (loadByte(addr + 1).toInt << 8) |
      loadByte(addr).toInt

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
