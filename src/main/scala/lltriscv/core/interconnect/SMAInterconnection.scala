package lltriscv.core.interconnect

import chisel3._
import chisel3.util._
import lltriscv.utils.CoreUtils
import lltriscv.bus.SMAReaderIO
import lltriscv.core.record.StoreQueueBypassIO
import lltriscv.core.execute.MemoryAccessLength

/*
 * SMA interconnect
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** SMA with store queue interconnect
  *
  * The read output port of the Memory executing component, bypassing store queue
  */
class SMAWithStoreQueueInterconnect extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SMAReaderIO())
    val out = new SMAReaderIO()
    val bypass = new StoreQueueBypassIO()
  })

  io.out.valid := io.in.valid
  io.out.address := io.in.address
  io.out.readType := io.in.readType

  io.in.error := io.out.error
  io.in.ready := io.out.ready

  // Bypass
  io.bypass.address := io.in.address
  private val data = VecInit(io.out.data(7, 0), io.out.data(15, 8), io.out.data(23, 16), io.out.data(31, 24))
  when(io.bypass.strobe(0)) {
    data(0) := io.bypass.data(7, 0)
  }
  when(io.bypass.strobe(1)) {
    data(1) := io.bypass.data(15, 8)
  }
  when(io.bypass.strobe(2)) {
    data(2) := io.bypass.data(23, 16)
  }
  when(io.bypass.strobe(3)) {
    data(3) := io.bypass.data(31, 24)
  }

  io.in.data := data(3) ## data(2) ## data(1) ## data(0)
}

/** SMA 2-readers interconnect
  *
  * Adopting priority arbitration, in1 > in2
  */
class SMA2ReaderInterconnect extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(new SMAReaderIO())
    val in2 = Flipped(new SMAReaderIO())
    val out = new SMAReaderIO()
  })
  private object Status extends ChiselEnum {
    val idle, pending1, pending2 = Value
  }
  private val statusReg = RegInit(Status.idle)

  io.in1.ready := false.B
  io.in1.data := 0.U
  io.in1.error := false.B

  io.in2.ready := false.B
  io.in2.data := 0.U
  io.in2.error := false.B

  io.out.valid := false.B
  io.out.address := 0.U
  io.out.readType := MemoryAccessLength.byte

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
