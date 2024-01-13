package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.broadcast.{DataBroadcastSlotEntry, DataBroadcastIO}
import lltriscv.utils.CoreUtils

/*
 * Register mapping table
 *
 * Register mapping table is responsible for executing register renaming logic and requesting space from ROB
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Register mapping table
  *
  * Perform 2-ways register renaming
  */
class RegisterMappingTable extends Module {
  val io = IO(new Bundle {
    // Mapping interface
    val mapping = Flipped(new RegisterMappingIO())
    // ROB alloc interface
    val alloc = Flipped(DecoupledIO(DataType.receipt))
    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())
    // Update interface
    val update = Flipped(new RegisterUpdateIO())
    // Recovery interface
    val recover = Input(Bool())
  })

  // TODO:  Building register types
  private val table = Reg(Vec(32, new RegisterMappingTableEntry()))

  io.mapping.ready := io.alloc.valid // Alloc valid
  io.alloc.ready := io.mapping.valid // Mapping valid

  /*---------------------------------Mapping logic start-------------------------------*/

  // 0: rs1
  when(io.mapping.regGroup(0).rs1 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(0).rs1.pending := false.B
    io.mapping.mappingGroup(0).rs1.receipt := 0.U
  }.otherwise {
    // Broadcast bypass
    for (i <- 0 until 2)
      io.mapping.mappingGroup(0).rs1 := CoreUtils.bypassBroadcast(
        table(io.mapping.regGroup(0).rs1).content,
        io.broadcast.entries(i)
      )
  }

  // 0: rs2
  when(io.mapping.regGroup(0).rs2 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(0).rs2.pending := false.B
    io.mapping.mappingGroup(0).rs2.receipt := 0.U
  }.otherwise {
    // Broadcast bypass
    for (i <- 0 until 2)
      io.mapping.mappingGroup(0).rs2 := CoreUtils.bypassBroadcast(
        table(io.mapping.regGroup(0).rs2).content,
        io.broadcast.entries(i)
      )
  }

  // 0: rd
  io.mapping.mappingGroup(0).rd := io.alloc.bits(30, 0) ## 0.U

  // 1: rs1
  when(io.mapping.regGroup(1).rs1 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(1).rs1.pending := false.B
    io.mapping.mappingGroup(1).rs1.receipt := 0.U
  }.elsewhen(io.mapping.regGroup(1).rs1 === io.mapping.regGroup(0).rd) { // rd bypass
    io.mapping.mappingGroup(1).rs1.pending := true.B
    io.mapping.mappingGroup(1).rs1.receipt := io.mapping.mappingGroup(0).rd
  }.otherwise {
    // Broadcast bypass
    for (i <- 0 until 2)
      io.mapping.mappingGroup(1).rs1 := CoreUtils.bypassBroadcast(
        table(io.mapping.regGroup(1).rs1).content,
        io.broadcast.entries(i)
      )
  }

  // 1: rs2
  when(io.mapping.regGroup(1).rs2 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(1).rs2.pending := false.B
    io.mapping.mappingGroup(1).rs2.receipt := 0.U
  }.elsewhen(io.mapping.regGroup(1).rs2 === io.mapping.regGroup(0).rd) { // rd bypass
    io.mapping.mappingGroup(1).rs2.pending := true.B
    io.mapping.mappingGroup(1).rs2.receipt := io.mapping.mappingGroup(0).rd
  }.otherwise {
    // Broadcast bypass
    for (i <- 0 until 2)
      io.mapping.mappingGroup(1).rs2 := CoreUtils.bypassBroadcast(
        table(io.mapping.regGroup(1).rs2).content,
        io.broadcast.entries(i)
      )
  }

  // 1: rd
  io.mapping.mappingGroup(1).rd := io.alloc.bits(30, 0) ## 1.U

  /*---------------------------------Mapping logic end-------------------------------*/

  /*---------------------------------Table logic start-------------------------------*/
  // Broadcast logic
  for (
    i <- 0 until 32;
    j <- 0 until 2
  ) {

    CoreUtils.matchBroadcast(table(i).content, io.broadcast.entries(j))
  }

  // Write table
  when(io.mapping.valid && io.mapping.ready) {
    for (i <- 0 until 2) {
      table(io.mapping.regGroup(i).rd).content.pending := true.B
      table(io.mapping.regGroup(i).rd).content.receipt := io.mapping
        .mappingGroup(i)
        .rd
    }
  }

  // Recovery logic
  when(io.recover) {
    // Debug printf
    printf("------Register Recover Start------\n")

    for (i <- 0 until 32) {
      printf("r%d = %d\n", i.U, table(i).recover)
    }
    for (i <- 0 until 2) {
      printf(
        "Update r%d = %d\n",
        io.update.entries(i).rd,
        io.update
          .entries(i)
          .result
      )
    }

    table.foreach(item => {
      item.content.pending := false.B
      item.content.receipt := item.recover
    })
  }

  // Update logic
  for (i <- 0 until 2)
    table(io.update.entries(i).rd).recover := io.update.entries(i).result

  /*---------------------------------Table logic end-------------------------------*/
}
