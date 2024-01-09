package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core._

class ROB(depth: Int) extends Module {
  val io = IO(new Bundle {
    val retired = DecoupledIO(DataType.receiptType)
    val alloc = DecoupledIO(DataType.receiptType)
  })

  def counter(depth: Int, incr: Bool): (UInt, UInt) = {
    val cntReg = RegInit(0.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
    when(incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)

  io.alloc.valid := !fullReg
  io.retired.valid := !emptyReg

  io.alloc.bits := writePtr
  io.retired.bits := readPtr

  val op =
    (io.alloc.valid && io.alloc.ready) ## (io.retired.valid && io.retired.ready)

  switch(op) {
    is("b01".U) { // retired/read
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
    }
    is("b10".U) { // alloc/write
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
    }
    is("b11".U) { // retired-alloc/read-write
      incrRead := true.B
      incrWrite := true.B
    }
  }
}
