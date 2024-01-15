package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.utils.CoreUtils

/*
 * ROB (ReOrder Buffer)
 *
 * The remapped instruction will enter ROB.
 * In ROB, save the status of instructions in the execution core and pipeline.
 * After the instruction is committed, it will retire from the ROB or trigger an event.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ROB (ReOrder Buffer) component
  *
  * Implementation using read/write pointer queue
  *
  * @param depth
  *   The number of ROB table items, each table entry stores 2 instructions.
  */
class ROB(depth: Int) extends Module {
  require(depth > 0, "ROB table depth must be greater than 0")
  val io = IO(new Bundle {
    // Retire interface
    val retired = DecoupledIO(DataType.receipt)

    /*
     * Alloc interface
     * Return the primary receipt of the table item.
     * The receipts of two table entries are (primary receipt << 1 | 1, primary receipt << 1 | 0)
     */
    val alloc = DecoupledIO(DataType.receipt)

    // ROB table interfaces
    val tableWrite = Flipped(new ROBTableWriteIO())
    val tableCommit = Flipped(new ROBTableCommitIO())
    val tableRetire = new ROBTableRetireIO(depth)

    // Recovery interface
    val recover = Input(Bool())
  })

  // Table logic
  private val table = Reg(Vec(depth * 2, new ROBTableEntry()))

  // Table write logic
  when(io.tableWrite.wen) {
    for (i <- 0 until 2) {
      table(io.tableWrite.entries(i).id).pc := io.tableWrite.entries(i).pc
      table(io.tableWrite.entries(i).id).spec := io.tableWrite.entries(i).spec
      table(io.tableWrite.entries(i).id).rd := io.tableWrite.entries(i).rd
      table(io.tableWrite.entries(i).id).valid := io.tableWrite.entries(i).valid
      table(io.tableWrite.entries(i).id).commit := false.B
    }
  }

  // Table commit logic
  for (i <- 0 until 2) {
    when(io.tableCommit.entries(i).valid) {
      table(io.tableCommit.entries(i).rd).executeResult := io.tableCommit
        .entries(i)
    }
  }

  // Table retire logic
  io.tableRetire.entries := table

  // Queue logic
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = CoreUtils.pointer(depth, incrRead)
  val (writePtr, nextWrite) = CoreUtils.pointer(depth, incrWrite)

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

  // Recovery logic
  when(io.recover) {
    table.foreach(_.valid := false.B)
  }
}
