package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record.RegisterMappingIO

/*
 * The instruction decoding stage is divided into three cycles:
 * DecodeStage -> RegisterMappingStage -> IssueStage
 *
 * DecodeStage: Split according to instruction format
 * RegisterMappingStage: Get and allocate renaming registers and ROB
 * IssueStage: Instruction queue arbitration
 */

class DecodeStage extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, new DecodeStageEntry()))
    val out = Output(Vec(2, new RegisterMappingStageEntry()))

    val nextReady = Input(Bool())
    val ready = Output(Bool())
  })
  private val decodeReg = Reg(Vec(2, new RegisterMappingStageEntry()))

  io.ready := io.nextReady

  when(io.nextReady) {
    for (i <- 0 until 2) {
      // opcode
      decodeReg(i).opcode := io.in(i).instruction(6, 0)
      // Instruction Type
      val instructionType = WireInit(InstructionType.UK)
      switch(io.in(i).instruction(6, 2)) {
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
      decodeReg(i).instructionType := instructionType

      // rd: R/I/U/J
      when(
        instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.U || instructionType === InstructionType.J
      ) {
        decodeReg(i).rd := io.in(i).instruction(11, 7)
      }.otherwise {
        decodeReg(i).rd := 0.U
      }

      // rs1: R/I/S/B
      when(
        instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.S || instructionType === InstructionType.B
      ) {
        decodeReg(i).rs1 := io.in(i).instruction(19, 15)
      }.otherwise {
        decodeReg(i).rs1 := 0.U
      }

      // rs2: R/S/B
      when(
        instructionType === InstructionType.R || instructionType === InstructionType.S || instructionType === InstructionType.B
      ) {
        decodeReg(i).rs2 := io.in(i).instruction(24, 20)
      }.otherwise {
        decodeReg(i).rs2 := 0.U
      }

      // func3: R/I/S/B
      when(
        instructionType === InstructionType.R || instructionType === InstructionType.I || instructionType === InstructionType.S || instructionType === InstructionType.B
      ) {
        decodeReg(i).func3 := io.in(i).instruction(14, 12)
      }.otherwise {
        decodeReg(i).func3 := 0.U
      }

      // func7: R
      when(
        instructionType === InstructionType.R
      ) {
        decodeReg(i).func7 := io.in(i).instruction(31, 25)
      }.otherwise {
        decodeReg(i).func7 := 0.U
      }

      // imm
      decodeReg(i).imm := 0.U
      switch(instructionType) {
        is(InstructionType.I) {
          decodeReg(i).imm := 0.U(20.W) ## io.in(i).instruction(31, 20)
        }
        is(InstructionType.S) {
          decodeReg(i).imm := 0.U(20.W) ##
            io.in(i).instruction(31, 25) ##
            io.in(i).instruction(11, 7)
        }
        is(InstructionType.B) {
          decodeReg(i).imm := 0.U(19.W) ##
            io.in(i).instruction(31) ##
            io.in(i).instruction(7) ##
            io.in(i).instruction(30, 25) ##
            io.in(i).instruction(11, 8) ##
            0.U(1.W)
        }
        is(InstructionType.U) {
          decodeReg(i).imm := io.in(i).instruction(31, 12) ##
            0.U(12.W)
        }
        is(InstructionType.J) {
          decodeReg(i).imm := 0.U(11.W)
          io.in(i).instruction(31) ##
            io.in(i).instruction(19, 12) ##
            io.in(i).instruction(20) ##
            io.in(i).instruction(30, 21) ##
            0.U(1.W)
        }
      }

      // Execute queue
      when(!io.in(i).vaild) {
        decodeReg(i).executeQueue := ExecuteQueueType.none
      }.otherwise {
        decodeReg(i).executeQueue := ExecuteQueueType.alu
      }

      decodeReg(i).vaild := io.in(i).vaild
      decodeReg(i).pc := io.in(i).pc
    }
  }

  io.out := decodeReg
}

class RegisterMappingStage extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, new RegisterMappingStageEntry()))
    val out = Output(Vec(2, new IssueStageEntry()))

    val nextReady = Input(Bool())
    val ready = Output(Bool())

    val mapping = new RegisterMappingIO()
  })

  io.mapping.valid := io.nextReady
  io.ready := io.mapping.ready && io.nextReady

  private val outReg = Reg(Vec(2, new IssueStageEntry()))

  for (i <- 0 until 2) {
    io.mapping.regGroup(i).rs1 := Mux(io.in(i).vaild, io.in(i).rs1, 0.U)
    io.mapping.regGroup(i).rs2 := Mux(io.in(i).vaild, io.in(i).rs2, 0.U)
    io.mapping.regGroup(i).rd := Mux(io.in(i).vaild, io.in(i).rd, 0.U)
  }

  when(io.nextReady) {
    when(io.mapping.ready) {
      for (i <- 0 until 2) {
        outReg(i).opcode := io.in(i).opcode
        outReg(i).instructionType := io.in(i).instructionType
        outReg(i).rs1 := io.mapping.mappingGroup(i).rs1
        outReg(i).rs2 := io.mapping.mappingGroup(i).rs2
        outReg(i).rd := io.mapping.mappingGroup(i).rd
        outReg(i).func3 := io.in(i).func3
        outReg(i).func7 := io.in(i).func7
        outReg(i).imm := io.in(i).imm
        outReg(i).pc := io.in(i).pc
        outReg(i).executeQueue := io.in(i).executeQueue
        outReg(i).vaild := io.in(i).vaild
      }
    }.otherwise {
      for (i <- 0 until 2) {
        outReg(i).executeQueue := ExecuteQueueType.none
        outReg(i).vaild := false.B
      }
    }
  }

  io.out := outReg
}

/** Issue stage:
  *
  * Select executable instructions to the instruction queue.When the execution
  * queue types of two instructions are different, two instructions can be
  * issued at once.If two instructions are the same type, will be issued in two
  * cycles.
  */
class IssueStage extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, new IssueStageEntry()))
    val ready = Output(Bool())

    /* Execute queue interface */
    val memoryEnq = DecoupledIO(new ExecuteEntry())
    val aluEnq = DecoupledIO(new ExecuteEntry())
    val branchEnq = DecoupledIO(new ExecuteEntry())
  })

  /** IssueStatus
    *
    * both: Both require issuing
    *
    * first: Only the first one requires issuing
    *
    * second: Only the second one requires issuing
    */
  private object IssueStatus extends ChiselEnum {
    val both, first, second = Value
  }

  private val issueStageReg = RegInit(IssueStatus.both)

  private val requireQueue =
    VecInit(io.in(0).executeQueue, io.in(1).executeQueue)

  private val queueReady = VecInit(false.B, false.B)
  for (i <- 0 until 2) {
    switch(requireQueue(i)) {
      is(ExecuteQueueType.memory) {
        queueReady(i) := io.memoryEnq.ready
      }
      is(ExecuteQueueType.alu) {
        queueReady(i) := io.aluEnq.ready
      }
      is(ExecuteQueueType.branch) {
        queueReady(i) := io.branchEnq.ready
      }
      is(ExecuteQueueType.none) {
        queueReady(i) := true.B
      }
    }
  }

  // Valid queue logic
  private def validQueue(id: Int) = {
    switch(requireQueue(id)) {
      is(ExecuteQueueType.memory) {
        io.memoryEnq.valid := true.B
        io.memoryEnq.bits <> io.in(id)
      }
      is(ExecuteQueueType.alu) {
        io.aluEnq.valid := true.B
        io.aluEnq.bits <> io.in(id)
      }
      is(ExecuteQueueType.branch) {
        io.branchEnq.valid := true.B
        io.branchEnq.bits <> io.in(id)
      }
    }
  }

  io.memoryEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  io.memoryEnq.valid := false.B
  io.aluEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  io.aluEnq.valid := false.B
  io.branchEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  io.branchEnq.valid := false.B

  when(issueStageReg === IssueStatus.both) {
    when(
      requireQueue(0) === requireQueue(1) &&
        requireQueue(0) =/= ExecuteQueueType.none
    ) {
      // Same type
      validQueue(0)
    }.otherwise {
      // Parallel
      validQueue(0)
      validQueue(1)
    }
  }.elsewhen(issueStageReg === IssueStatus.first) {
    validQueue(0)
  }.elsewhen(issueStageReg === IssueStatus.second) {
    validQueue(1)
  }

  // Status logic
  io.ready := false.B
  when(issueStageReg === IssueStatus.both) {
    when(
      requireQueue(0) === requireQueue(1) &&
        requireQueue(0) =/= ExecuteQueueType.none
    ) {
      // Same type
      io.ready := false.B
      when(queueReady(0)) {
        issueStageReg := IssueStatus.second
      }.otherwise {
        issueStageReg := IssueStatus.both
      }
    }.elsewhen(queueReady(0) && queueReady(1)) {
      // Both OK
      io.ready := true.B
      issueStageReg := IssueStatus.both
    }.elsewhen(queueReady(0)) {
      // First OK
      io.ready := false.B
      issueStageReg := IssueStatus.second
    }.elsewhen(queueReady(1)) {
      // Second OK
      io.ready := false.B
      issueStageReg := IssueStatus.first
    }
  }.elsewhen(issueStageReg === IssueStatus.first) {
    io.ready := queueReady(0)
    when(io.ready) {
      issueStageReg := IssueStatus.both
    }.otherwise {
      issueStageReg := IssueStatus.first
    }
  }.elsewhen(issueStageReg === IssueStatus.second) {
    io.ready := queueReady(1)
    when(io.ready) {
      issueStageReg := IssueStatus.both
    }.otherwise {
      issueStageReg := IssueStatus.second
    }
  }
}
