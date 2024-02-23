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

    val hit = Input(Bool())
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

  // Print

  when(io.hit) {
    printf(
      "Registers[>]R=ra:%x(%x) sp:%x(%x) gp:%x(%x) tp:%x(%x) t0:%x(%x) t1:%x(%x) t2:%x(%x) s0/fp:%x(%x) s1:%x(%x) a0:%x(%x) a1:%x(%x) a2:%x(%x) a3:%x(%x) a4:%x(%x) a5:%x(%x) a6:%x(%x) a7:%x(%x) s2:%x(%x) s3:%x(%x) s4:%x(%x) s5:%x(%x) s6:%x(%x) s7:%x(%x) s8:%x(%x) s9:%x(%x) s10:%x(%x) s11:%x(%x) t3:%x(%x) t4:%x(%x) t5:%x(%x) t6:%x(%x)\n",
      table(1).recover,
      Mux(table(1).content.pending, table(1).content.receipt, 0.U),
      table(2).recover,
      Mux(table(2).content.pending, table(2).content.receipt, 0.U),
      table(3).recover,
      Mux(table(3).content.pending, table(3).content.receipt, 0.U),
      table(4).recover,
      Mux(table(4).content.pending, table(4).content.receipt, 0.U),
      table(5).recover,
      Mux(table(5).content.pending, table(5).content.receipt, 0.U),
      table(6).recover,
      Mux(table(6).content.pending, table(6).content.receipt, 0.U),
      table(7).recover,
      Mux(table(7).content.pending, table(7).content.receipt, 0.U),
      table(8).recover,
      Mux(table(8).content.pending, table(8).content.receipt, 0.U),
      table(9).recover,
      Mux(table(9).content.pending, table(9).content.receipt, 0.U),
      table(10).recover,
      Mux(table(10).content.pending, table(10).content.receipt, 0.U),
      table(11).recover,
      Mux(table(11).content.pending, table(11).content.receipt, 0.U),
      table(12).recover,
      Mux(table(12).content.pending, table(12).content.receipt, 0.U),
      table(13).recover,
      Mux(table(13).content.pending, table(13).content.receipt, 0.U),
      table(14).recover,
      Mux(table(14).content.pending, table(14).content.receipt, 0.U),
      table(15).recover,
      Mux(table(15).content.pending, table(15).content.receipt, 0.U),
      table(16).recover,
      Mux(table(16).content.pending, table(16).content.receipt, 0.U),
      table(17).recover,
      Mux(table(17).content.pending, table(17).content.receipt, 0.U),
      table(18).recover,
      Mux(table(18).content.pending, table(18).content.receipt, 0.U),
      table(19).recover,
      Mux(table(19).content.pending, table(19).content.receipt, 0.U),
      table(20).recover,
      Mux(table(20).content.pending, table(20).content.receipt, 0.U),
      table(21).recover,
      Mux(table(21).content.pending, table(21).content.receipt, 0.U),
      table(22).recover,
      Mux(table(22).content.pending, table(22).content.receipt, 0.U),
      table(23).recover,
      Mux(table(23).content.pending, table(23).content.receipt, 0.U),
      table(24).recover,
      Mux(table(24).content.pending, table(24).content.receipt, 0.U),
      table(25).recover,
      Mux(table(25).content.pending, table(25).content.receipt, 0.U),
      table(26).recover,
      Mux(table(26).content.pending, table(26).content.receipt, 0.U),
      table(27).recover,
      Mux(table(27).content.pending, table(27).content.receipt, 0.U),
      table(28).recover,
      Mux(table(28).content.pending, table(28).content.receipt, 0.U),
      table(29).recover,
      Mux(table(29).content.pending, table(29).content.receipt, 0.U),
      table(30).recover,
      Mux(table(30).content.pending, table(30).content.receipt, 0.U),
      table(31).recover,
      Mux(table(31).content.pending, table(31).content.receipt, 0.U)
    )
  }
}
