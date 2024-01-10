package lltriscv.core.record

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.broadcast.{DataBroadcastSlotEntry, DataBroadcastIO}

class RegisterMappingIO extends Bundle {
  val regGroup = Output(
    Vec(
      2,
      new Bundle {
        val rs1 = DataType.registerType.cloneType
        val rs2 = DataType.registerType.cloneType
        val rd = DataType.registerType.cloneType
      }
    )
  )
  val mappingGroup = Input(
    Vec(
      2,
      new Bundle {
        val rs1 = new DataBroadcastSlotEntry()
        val rs2 = new DataBroadcastSlotEntry()
        val rd = DataType.receiptType.cloneType
      }
    )
  )
  val valid = Output(Bool())
  val ready = Input(Bool())
}

class RegisterMappingTableEntry extends Bundle {
  val busy = Bool()
  val receipt = DataType.receiptType.cloneType
}

class RegisterMappingTable extends Module {
  val io = IO(new Bundle {
    val mapping = Flipped(new RegisterMappingIO())
    val alloc = Flipped(DecoupledIO(DataType.receiptType))
    val broadcast = Flipped(new DataBroadcastIO())
  })

  private val table = Reg(Vec(32, new RegisterMappingTableEntry()))

  io.mapping.ready := io.alloc.valid
  io.alloc.ready := io.mapping.valid

  /*---------------------------------Mapping logic start-------------------------------*/

  // 0: rs1
  when(io.mapping.regGroup(0).rs1 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(0).rs1.pending := false.B
    io.mapping.mappingGroup(0).rs1.receipt := 0.U
  }.elsewhen(table(io.mapping.regGroup(0).rs1).busy) { // busy
    when(
      table(io.mapping.regGroup(0).rs1).receipt ===
        io.broadcast.entries(0).receipt &&
        io.broadcast.entries(0).valid
    ) { // broadcast0 bypass
      io.mapping.mappingGroup(0).rs1.pending := false.B
      io.mapping.mappingGroup(0).rs1.receipt := io.broadcast.entries(0).data
    }.elsewhen(
      table(io.mapping.regGroup(0).rs1).receipt ===
        io.broadcast.entries(1).receipt &&
        io.broadcast.entries(1).valid
    ) { // broadcast1 bypass
      io.mapping.mappingGroup(0).rs1.pending := false.B
      io.mapping.mappingGroup(0).rs1.receipt := io.broadcast.entries(1).data
    }.otherwise { // pending
      io.mapping.mappingGroup(0).rs1.pending := true.B
      io.mapping
        .mappingGroup(0)
        .rs1
        .receipt := table(io.mapping.regGroup(0).rs1).receipt
    }
  }.otherwise { // valid
    io.mapping.mappingGroup(0).rs1.pending := false.B
    io.mapping.mappingGroup(0).rs1.receipt := table(
      io.mapping.regGroup(0).rs1
    ).receipt
  }

  // 0: rs2
  when(io.mapping.regGroup(0).rs2 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(0).rs2.pending := false.B
    io.mapping.mappingGroup(0).rs2.receipt := 0.U
  }.elsewhen(table(io.mapping.regGroup(0).rs2).busy) { // busy
    when(
      table(io.mapping.regGroup(0).rs2).receipt ===
        io.broadcast.entries(0).receipt &&
        io.broadcast.entries(0).valid
    ) { // broadcast0 bypass
      io.mapping.mappingGroup(0).rs2.pending := false.B
      io.mapping.mappingGroup(0).rs2.receipt := io.broadcast.entries(0).data
    }.elsewhen(
      table(io.mapping.regGroup(0).rs2).receipt ===
        io.broadcast.entries(1).receipt &&
        io.broadcast.entries(1).valid
    ) { // broadcast1 bypass
      io.mapping.mappingGroup(0).rs2.pending := false.B
      io.mapping.mappingGroup(0).rs2.receipt := io.broadcast.entries(1).data
    }.otherwise { // pending
      io.mapping.mappingGroup(0).rs2.pending := true.B
      io.mapping
        .mappingGroup(0)
        .rs2
        .receipt := table(io.mapping.regGroup(0).rs2).receipt
    }
  }.otherwise { // valid
    io.mapping.mappingGroup(0).rs2.pending := false.B
    io.mapping.mappingGroup(0).rs2.receipt := table(
      io.mapping.regGroup(0).rs2
    ).receipt
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
  }.elsewhen(table(io.mapping.regGroup(1).rs1).busy) { // busy
    when(
      table(io.mapping.regGroup(1).rs1).receipt ===
        io.broadcast.entries(0).receipt &&
        io.broadcast.entries(0).valid
    ) { // broadcast0 bypass
      io.mapping.mappingGroup(1).rs1.pending := false.B
      io.mapping.mappingGroup(1).rs1.receipt := io.broadcast.entries(0).data
    }.elsewhen(
      table(io.mapping.regGroup(1).rs1).receipt ===
        io.broadcast.entries(1).receipt &&
        io.broadcast.entries(1).valid
    ) { // broadcast1 bypass
      io.mapping.mappingGroup(1).rs1.pending := false.B
      io.mapping.mappingGroup(1).rs1.receipt := io.broadcast.entries(1).data
    }.otherwise { // pending
      io.mapping.mappingGroup(1).rs1.pending := true.B
      io.mapping
        .mappingGroup(1)
        .rs1
        .receipt := table(io.mapping.regGroup(1).rs1).receipt
    }
  }.otherwise { // valid
    io.mapping.mappingGroup(1).rs1.pending := false.B
    io.mapping.mappingGroup(1).rs1.receipt := table(
      io.mapping.regGroup(1).rs1
    ).receipt
  }

  // 1: rs2
  when(io.mapping.regGroup(1).rs2 === 0.U) { // x0 bypass
    io.mapping.mappingGroup(1).rs2.pending := false.B
    io.mapping.mappingGroup(1).rs2.receipt := 0.U
  }.elsewhen(io.mapping.regGroup(1).rs2 === io.mapping.regGroup(0).rd) { // rd bypass
    io.mapping.mappingGroup(1).rs2.pending := true.B
    io.mapping.mappingGroup(1).rs2.receipt := io.mapping.mappingGroup(0).rd
  }.elsewhen(table(io.mapping.regGroup(1).rs2).busy) { // busy
    when(
      table(io.mapping.regGroup(1).rs2).receipt ===
        io.broadcast.entries(0).receipt &&
        io.broadcast.entries(0).valid
    ) { // broadcast0 bypass
      io.mapping.mappingGroup(1).rs2.pending := false.B
      io.mapping.mappingGroup(1).rs2.receipt := io.broadcast.entries(0).data
    }.elsewhen(
      table(io.mapping.regGroup(1).rs2).receipt ===
        io.broadcast.entries(1).receipt &&
        io.broadcast.entries(1).valid
    ) { // broadcast1 bypass
      io.mapping.mappingGroup(1).rs2.pending := false.B
      io.mapping.mappingGroup(1).rs2.receipt := io.broadcast.entries(1).data
    }.otherwise { // pending
      io.mapping.mappingGroup(1).rs2.pending := true.B
      io.mapping
        .mappingGroup(1)
        .rs2
        .receipt := table(io.mapping.regGroup(1).rs2).receipt
    }
  }.otherwise { // valid
    io.mapping.mappingGroup(1).rs2.pending := false.B
    io.mapping.mappingGroup(1).rs2.receipt := table(
      io.mapping.regGroup(1).rs2
    ).receipt
  }

  // 1: rd
  io.mapping.mappingGroup(1).rd := io.alloc.bits(30, 0) ## 1.U

  /*---------------------------------Mapping logic end-------------------------------*/

  /*---------------------------------Table logic start-------------------------------*/

  when(io.mapping.valid && io.mapping.ready) {
    when(io.mapping.regGroup(0).rd === io.mapping.regGroup(1).rd) {
      table(io.mapping.regGroup(1).rd).busy := true.B
      table(io.mapping.regGroup(1).rd).receipt := io.mapping.mappingGroup(1).rd
    }.otherwise {
      table(io.mapping.regGroup(0).rd).busy := true.B
      table(io.mapping.regGroup(0).rd).receipt := io.mapping.mappingGroup(0).rd
      table(io.mapping.regGroup(1).rd).busy := true.B
      table(io.mapping.regGroup(1).rd).receipt := io.mapping.mappingGroup(1).rd
    }

    for (i <- 0 until 32) {
      when(
        io.broadcast.entries(0).receipt === table(i).receipt &&
          i.U =/= io.mapping.regGroup(0).rd &&
          i.U =/= io.mapping.regGroup(1).rd &&
          table(i).busy &&
          io.broadcast.entries(0).valid
      ) {
        table(i).busy := false.B
        table(i).receipt := io.broadcast.entries(0).data
      }.elsewhen(
        io.broadcast.entries(1).receipt === table(i).receipt &&
          i.U =/= io.mapping.regGroup(0).rd &&
          i.U =/= io.mapping.regGroup(1).rd &&
          table(i).busy &&
          io.broadcast.entries(1).valid
      ) {
        table(i).busy := false.B
        table(i).receipt := io.broadcast.entries(1).data
      }
    }
  }

  /*---------------------------------Table logic end-------------------------------*/
}
