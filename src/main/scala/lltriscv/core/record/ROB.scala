package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core._

class ROBTableEntry extends Bundle {
  val result = DataType.operationType.cloneType
  val pc = DataType.pcType.cloneType
  val commit = Bool()
  val valid = Bool()
}

class ROBTableWriteIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val id = DataType.receiptType.cloneType
        val pc = DataType.pcType.cloneType
        val valid = Bool()
      }
    )
  )
  val wen = Output(Bool())
}

class ROBTableCommitIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val id = DataType.receiptType.cloneType
        val result = DataType.operationType.cloneType
        val valid = Bool()
      }
    )
  )
  val wen = Output(Bool())
}

class ROBTableRetireIO(depth: Int) extends Bundle {
  val entries = Output(Vec(depth * 2, new ROBTableEntry()))
}

class ROB(depth: Int) extends Module {
  val io = IO(new Bundle {
    val retired = DecoupledIO(DataType.receiptType)
    val alloc = DecoupledIO(DataType.receiptType)

    val tableWrite = Flipped(new ROBTableWriteIO())
    val tableCommit = Flipped(new ROBTableCommitIO())
    val tableRetire = new ROBTableRetireIO(depth)
  })
  // Table logic
  private val table = Reg(Vec(depth * 2, new ROBTableEntry()))

  when(io.tableWrite.wen) {
    for (i <- 0 until 2) {
      table(io.tableWrite.entries(i).id).pc := io.tableWrite.entries(i).pc
      table(io.tableWrite.entries(i).id).valid := io.tableWrite.entries(i).valid
      table(io.tableWrite.entries(i).id).commit := false.B
    }
  }

  when(io.tableCommit.wen) {
    for (i <- 0 until 2) {
      when(io.tableCommit.entries(i).valid) {
        table(io.tableCommit.entries(i).id).result := io.tableCommit
          .entries(i)
          .result
        table(io.tableCommit.entries(i).id).commit := true.B
      }
    }
  }

  io.tableRetire.entries := table

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
