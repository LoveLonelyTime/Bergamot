package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Register mapping table
 *
 * Register mapping table is responsible for executing register renaming logic and requesting space from ROB
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Register mapping table
  *
  * 2-ways register renaming
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

  // Register files
  private val table = RegInit(Vec(32, new RegisterMappingTableEntry()).zero)

  io.mapping.ready := io.alloc.valid // Alloc valid
  io.alloc.ready := io.mapping.valid // Mapping valid

  // Mapping logic
  private val progressGroups = (0 until io.mapping.regGroup.length).map(io.mapping.mappingGroup.zip(io.mapping.regGroup).take(_))
  io.mapping.mappingGroup.zip(io.mapping.regGroup).zipWithIndex.foreach { case ((mapping, reg), i) =>
    val mappingPair = Seq(
      reg.rs1 -> mapping.rs1,
      reg.rs2 -> mapping.rs2,
      reg.rs3 -> mapping.rs3
    )
    mappingPair.foreach { mappingEntry =>
      when(mappingEntry._1 === 0.U) { // x0
        mappingEntry._2.pending := false.B
        mappingEntry._2.receipt := 0.U
      }.otherwise {
        // Original mapping
        mappingEntry._2 := table(mappingEntry._1).content

        // Broadcast bypass
        io.broadcast.entries.foreach { matchBroadcast(mappingEntry._2, table(mappingEntry._1).content, _) }

        // rd bypass
        progressGroups(i).foreach { case (progressMapping, progressReg) =>
          when(mappingEntry._1 === progressReg.rd) {
            mappingEntry._2.pending := true.B
            mappingEntry._2.receipt := progressMapping.rd
          }
        }
      }
    }
    // ROB ID | order
    mapping.rd := io.alloc.bits ## i.U
  }

  // Table logic start
  // Broadcast logic
  for (
    entry <- table;
    broadcast <- io.broadcast.entries
  ) {
    matchBroadcast(entry.content, entry.content, broadcast)
  }

  // Write table
  when(io.mapping.valid && io.mapping.ready) {
    io.mapping.mappingGroup.zip(io.mapping.regGroup).foreach { case (mapping, reg) =>
      table(reg.rd).content.pending := true.B
      table(reg.rd).content.receipt := mapping.rd
    }
  }

  // Recovery logic
  when(io.recover) {

    table.foreach { item =>
      item.content.pending := false.B
      item.content.receipt := item.recover
    }

    // Update bypass
    io.update.entries.foreach { item => table(item.rd).content.receipt := item.result }
  }

  // Update logic
  io.update.entries.foreach { item => table(item.rd).recover := item.result }
}
