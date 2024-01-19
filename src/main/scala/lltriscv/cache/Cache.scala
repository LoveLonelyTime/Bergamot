package lltriscv.cache

import chisel3._
import chisel3.util._
import lltriscv.bus.SMAReaderIO
import lltriscv.bus.SMAWriterIO

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
