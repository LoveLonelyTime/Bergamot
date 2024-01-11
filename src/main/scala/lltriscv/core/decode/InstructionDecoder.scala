package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.core.execute._
import lltriscv.core.broadcast._

/*
 * Decode stage
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Instruction decoder
  *
  * The instruction decoding stage is divided into three cycles: DecodeStage ->
  * RegisterMappingStage -> IssueStage
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class InstructionDecoder(executeQueueWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))

    // Mapping interface
    val mapping = new RegisterMappingIO()
    val broadcast = Flipped(new DataBroadcastIO())

    /* Execute queue interface */
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())

    val tableWrite = new ROBTableWriteIO()
  })
  private val decodeStage = Module(new DecodeStage())
  private val registerMappingStage = Module(new RegisterMappingStage())
  private val issueStage = Module(new IssueStage(executeQueueWidth))

  decodeStage.io.in <> io.in
  decodeStage.io.out <> registerMappingStage.io.in

  registerMappingStage.io.out <> issueStage.io.in
  registerMappingStage.io.mapping <> io.mapping
  registerMappingStage.io.tableWrite <> io.tableWrite

  issueStage.io.enqs <> io.enqs
  issueStage.io.broadcast <> io.broadcast

}

/** Decode stage
  *
  * Split according to instruction format
  */
class DecodeStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
    val out = DecoupledIO(Vec(2, new RegisterMappingStageEntry()))
  })

  private val inReg = Reg(Vec(2, new DecodeStageEntry()))

  // Pipeline logic
  io.in.ready := io.out.ready
  when(io.out.valid && io.out.ready) {
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }

  // Decode logic
  for (i <- 0 until 2) {
    // opcode
    io.out.bits(i).opcode := inReg(i).instruction(6, 0)
    // Instruction Type
    val instructionType = WireInit(InstructionType.UK)
    switch(inReg(i).instruction(6, 2)) {
      // I: lui
      is("b01101".U) {
        instructionType := InstructionType.U
      }
      // I: auipc
      is("b00101".U) {
        instructionType := InstructionType.U
      }
      // I: beq, bne, blt, bge, bltu, bgeu
      is("b11000".U) {
        instructionType := InstructionType.B
      }
      // I: sb, sh, sw
      is("b01000".U) {
        instructionType := InstructionType.S
      }
      // I: add, sub, sll, slt, sltu, xor, srl, sra, or, and
      is("b01100".U) {
        instructionType := InstructionType.R
      }
      // I: jal
      is("b11011".U) {
        instructionType := InstructionType.J
      }
      // I: lb, lh, lw, lbu, lhu
      is("b00000".U) {
        instructionType := InstructionType.I
      }
      // I: addi, slti, sltiu, xori, ori, andi, slli, srli, srai
      is("b00100".U) {
        instructionType := InstructionType.I
      }
      // I: jalr
      is("b11001".U) {
        instructionType := InstructionType.I
      }
      // I: fence, fence.i
      is("b00011".U) {
        instructionType := InstructionType.I
      }
      // I: ecall, ebreak, csrrw, csrrs, csrrc, csrrwi, csrrsi, csrrci
      is("b11100".U) {
        instructionType := InstructionType.I
      }
    }
    io.out.bits(i).instructionType := instructionType

    // rd: R/I/U/J
    when(
      instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.U || instructionType === InstructionType.J
    ) {
      io.out.bits(i).rd := inReg(i).instruction(11, 7)
    }.otherwise {
      io.out.bits(i).rd := 0.U
    }

    // rs1: R/I/S/B
    when(
      instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.S || instructionType === InstructionType.B
    ) {
      io.out.bits(i).rs1 := inReg(i).instruction(19, 15)
    }.otherwise {
      io.out.bits(i).rs1 := 0.U
    }

    // rs2: R/S/B
    when(
      instructionType === InstructionType.R || instructionType === InstructionType.S || instructionType === InstructionType.B
    ) {
      io.out.bits(i).rs2 := inReg(i).instruction(24, 20)
    }.otherwise {
      io.out.bits(i).rs2 := 0.U
    }

    // func3: R/I/S/B
    when(
      instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.S || instructionType === InstructionType.B
    ) {
      io.out.bits(i).func3 := inReg(i).instruction(14, 12)
    }.otherwise {
      io.out.bits(i).func3 := 0.U
    }

    // func7: R
    when(
      instructionType === InstructionType.R
    ) {
      io.out.bits(i).func7 := inReg(i).instruction(31, 25)
    }.otherwise {
      io.out.bits(i).func7 := 0.U
    }

    // imm
    io.out.bits(i).imm := 0.U
    switch(instructionType) {
      is(InstructionType.I) {
        io.out.bits(i).imm := 0.U(20.W) ## inReg(i).instruction(31, 20)
      }
      is(InstructionType.S) {
        io.out.bits(i).imm := 0.U(20.W) ##
          inReg(i).instruction(31, 25) ##
          inReg(i).instruction(11, 7)
      }
      is(InstructionType.B) {
        io.out.bits(i).imm := 0.U(19.W) ##
          inReg(i).instruction(31) ##
          inReg(i).instruction(7) ##
          inReg(i).instruction(30, 25) ##
          inReg(i).instruction(11, 8) ##
          0.U(1.W)
      }
      is(InstructionType.U) {
        io.out.bits(i).imm := inReg(i).instruction(31, 12) ##
          0.U(12.W)
      }
      is(InstructionType.J) {
        io.out.bits(i).imm := 0.U(11.W)
        inReg(i).instruction(31) ##
          inReg(i).instruction(19, 12) ##
          inReg(i).instruction(20) ##
          inReg(i).instruction(30, 21) ##
          0.U(1.W)
      }
    }

    // TODO: Execute queue arbitrate
    when(inReg(0).valid) {
      io.out.bits(0).executeQueue := ExecuteQueueType.alu
    }.otherwise {
      io.out.bits(0).executeQueue := ExecuteQueueType.none
    }
    when(inReg(1).valid) {
      io.out.bits(1).executeQueue := ExecuteQueueType.alu2
    }.otherwise {
      io.out.bits(1).executeQueue := ExecuteQueueType.none
    }

    io.out.bits(i).valid := inReg(i).valid
    io.out.bits(i).pc := inReg(i).pc
  }

  io.out.valid := true.B
}

/** Register mapping stage
  *
  * Getand allocate renaming registers and ROB
  */
class RegisterMappingStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new RegisterMappingStageEntry())))
    val out = DecoupledIO(Vec(2, new IssueStageEntry()))
    // Mapping interface
    val mapping = new RegisterMappingIO()

    // ROBTable interface
    val tableWrite = new ROBTableWriteIO()
  })

  private val inReg = Reg(Vec(2, new RegisterMappingStageEntry()))

  // Pipeline logic
  io.in.ready := io.mapping.ready && io.out.ready
  when(io.out.valid && io.out.ready) {
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }

  // Mapping logic
  io.mapping.valid := io.out.ready
  for (i <- 0 until 2) {
    io.mapping.regGroup(i).rs1 := Mux(
      inReg(i).valid,
      inReg(i).rs1,
      0.U
    )
    io.mapping.regGroup(i).rs2 := Mux(
      inReg(i).valid,
      inReg(i).rs2,
      0.U
    )
    io.mapping.regGroup(i).rd := Mux(inReg(i).valid, inReg(i).rd, 0.U)
  }

  for (i <- 0 until 2) {
    io.out.bits(i).opcode := inReg(i).opcode
    io.out.bits(i).instructionType := inReg(i).instructionType
    io.out.bits(i).executeQueue := inReg(i).executeQueue

    // Mapping result
    io.out.bits(i).rs1 := io.mapping.mappingGroup(i).rs1
    io.out.bits(i).rs2 := io.mapping.mappingGroup(i).rs2
    io.out.bits(i).rd := io.mapping.mappingGroup(i).rd

    io.out.bits(i).func3 := inReg(i).func3
    io.out.bits(i).func7 := inReg(i).func7
    io.out.bits(i).imm := inReg(i).imm

    io.out.bits(i).pc := inReg(i).pc
    io.out.bits(i).valid := inReg(i).valid

    io.tableWrite.entries(i).id := io.mapping.mappingGroup(i).rd
    io.tableWrite.entries(i).pc := inReg(i).pc
    io.tableWrite.entries(i).valid := inReg(i).valid
  }

  io.tableWrite.wen := io.mapping.valid && io.mapping.ready

  io.out.valid := io.mapping.ready
}

/** Issue stage
  *
  * Instruction queue arbitration
  *
  * Select executable instructions to the instruction queue.When the execution
  * queue types of two instructions are different, two instructions can be
  * issued at once.If two instructions are the same type, will be issued in two
  * cycles.
  */
class IssueStage(executeQueueWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new IssueStageEntry())))
    val broadcast = Flipped(new DataBroadcastIO())
    /* Execute queue interface */
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
  })

  private val inReg = Reg(Vec(2, new IssueStageEntry()))

  // Broadcast logic
  for (
    i <- 0 until 2;
    j <- 0 until 2
  ) {
    when(
      inReg(i).rs1.pending &&
        io.broadcast.entries(j).valid &&
        inReg(i).rs1.receipt === io.broadcast.entries(j).receipt
    ) { // rs1
      inReg(i).rs1.pending := false.B
      inReg(i).rs1.receipt := io.broadcast.entries(j).data
    }

    when(
      inReg(i).rs2.pending &&
        io.broadcast.entries(j).valid &&
        inReg(i).rs2.receipt === io.broadcast.entries(j).receipt
    ) { // rs2
      inReg(i).rs2.pending := false.B
      inReg(i).rs2.receipt := io.broadcast.entries(j).data
    }
  }

  // Check queue grants
  private val grants = VecInit(
    VecInit.fill(executeQueueWidth)(false.B),
    VecInit.fill(executeQueueWidth)(false.B)
  )
  private val queueReady = VecInit(true.B, true.B)

  io.enqs.foreach(_.enq.valid := false.B)

  for (i <- 0 until executeQueueWidth) {
    grants(0)(i) := inReg(0).executeQueue === io.enqs(i).queueType &&
      inReg(0).valid
    when(grants(0)(i)) {
      queueReady(0) := io.enqs(i).enq.ready
      io.enqs(i).enq.valid := true.B
    }
  }

  for (i <- 0 until executeQueueWidth) {
    grants(1)(i) := inReg(1).executeQueue === io.enqs(i).queueType &&
      inReg(1).valid &&
      !grants(0)(i) // priority

    when(grants(1)(i)) {
      queueReady(1) := io.enqs(i).enq.ready
      io.enqs(i).enq.valid := true.B
    }.elsewhen(
      inReg(1).executeQueue === io.enqs(i).queueType &&
        inReg(1).valid &&
        grants(0)(i)
    ) {
      queueReady(1) := false.B
    }
  }

  // TODO: connect
  for (i <- 0 until executeQueueWidth) {
    io.enqs(i).enq.bits := 0.U.asTypeOf(new ExecuteEntry())
  }

  for (
    i <- 0 until 2;
    j <- 0 until executeQueueWidth
  ) {
    when(grants(i)(j)) {
      io.enqs(j).enq.bits <> inReg(i)
    }
  }

  for (i <- 0 until 2) {
    when(queueReady(i)) {
      inReg(i).valid := false.B
    }
  }

  // Pipeline logic
  io.in.ready := queueReady(0) && queueReady(1)
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }
}
