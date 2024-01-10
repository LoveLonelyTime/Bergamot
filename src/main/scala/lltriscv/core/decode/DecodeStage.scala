package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.core.execute._
import lltriscv.core.broadcast._

/*
 * The instruction decoding stage is divided into three cycles:
 * DecodeStage -> RegisterMappingStage -> IssueStage
 *
 * DecodeStage: Split according to instruction format
 * RegisterMappingStage: Get and allocate renaming registers and ROB
 * IssueStage: Instruction queue arbitration
 */
class InstructionDecoder extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))

    // Mapping interface
    val mapping = new RegisterMappingIO()
    val broadcast = Flipped(new DataBroadcastIO())

    /* Execute queue interface */
    val memoryEnq = DecoupledIO(new ExecuteEntry())
    val aluEnq = DecoupledIO(new ExecuteEntry())
    val branchEnq = DecoupledIO(new ExecuteEntry())
  })
  private val decodeStage = Module(new DecodeStage())
  private val registerMappingStage = Module(new RegisterMappingStage())
  private val issueStage = Module(new IssueStage())

  decodeStage.io.in <> io.in
  decodeStage.io.out <> registerMappingStage.io.in
  registerMappingStage.io.out <> issueStage.io.in

  registerMappingStage.io.mapping <> io.mapping

  issueStage.io.memoryEnq <> io.memoryEnq
  issueStage.io.aluEnq <> io.aluEnq
  issueStage.io.branchEnq <> io.branchEnq

  issueStage.io.broadcast <> io.broadcast
}

class DecodeStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
    val out = DecoupledIO(Vec(2, new RegisterMappingStageEntry()))
  })

  private val inReg = Reg(Vec(2, new DecodeStageEntry()))
  // Pipeline logic
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }
  io.in.ready := io.out.ready

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
    when(!inReg(i).vaild) {
      io.out.bits(i).executeQueue := ExecuteQueueType.none
    }.otherwise {
      io.out.bits(i).executeQueue := ExecuteQueueType.alu
    }

    io.out.bits(i).vaild := inReg(i).vaild
    io.out.bits(i).pc := inReg(i).pc
  }

  io.out.valid := io.in.valid
}

class RegisterMappingStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new RegisterMappingStageEntry())))
    val out = DecoupledIO(Vec(2, new IssueStageEntry()))
    // Mapping interface
    val mapping = new RegisterMappingIO()
  })

  private val inReg = Reg(Vec(2, new RegisterMappingStageEntry()))
  // Pipeline logic
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }
  io.in.ready := io.mapping.ready && io.out.ready

  // Mapping logic
  io.mapping.valid := io.out.ready && io.in.valid
  for (i <- 0 until 2) {
    io.mapping.regGroup(i).rs1 := Mux(
      inReg(i).vaild,
      inReg(i).rs1,
      0.U
    )
    io.mapping.regGroup(i).rs2 := Mux(
      inReg(i).vaild,
      inReg(i).rs2,
      0.U
    )
    io.mapping.regGroup(i).rd := Mux(inReg(i).vaild, inReg(i).rd, 0.U)
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
    io.out.bits(i).vaild := inReg(i).vaild
  }

  io.out.valid := io.in.valid && io.mapping.valid
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
    val in = Flipped(DecoupledIO(Vec(2, new IssueStageEntry())))
    val broadcast = Flipped(new DataBroadcastIO())
    /* Execute queue interface */
    val memoryEnq = DecoupledIO(new ExecuteEntry())
    val aluEnq = DecoupledIO(new ExecuteEntry())
    val branchEnq = DecoupledIO(new ExecuteEntry())
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

  // Pipeline logic
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }

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

  // Check queue status
  private val requireQueue =
    VecInit(inReg(0).executeQueue, inReg(1).executeQueue)

  private val queueReady = VecInit(false.B, false.B)
  private val queueValid = VecInit(false.B, false.B)
  for (i <- 0 until 2) {
    switch(requireQueue(i)) {
      is(ExecuteQueueType.memory) {
        queueReady(i) := io.memoryEnq.ready
        queueValid(i) := io.memoryEnq.valid
      }
      is(ExecuteQueueType.alu) {
        queueReady(i) := io.aluEnq.ready
        queueValid(i) := io.aluEnq.valid
      }
      is(ExecuteQueueType.branch) {
        queueReady(i) := io.branchEnq.ready
        queueValid(i) := io.branchEnq.valid
      }
      is(ExecuteQueueType.none) {
        queueReady(i) := true.B
        queueValid(i) := true.B
      }
    }
  }

  // connect logic
  // TODO : connect
  private def connectQueue(id: Int) = {
    switch(requireQueue(id)) {
      is(ExecuteQueueType.memory) {
        io.memoryEnq.bits <> inReg(id)
      }
      is(ExecuteQueueType.alu) {
        io.aluEnq.bits <> inReg(id)
      }
      is(ExecuteQueueType.branch) {
        io.branchEnq.bits <> inReg(id)
      }
    }
  }

  io.memoryEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  io.aluEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  io.branchEnq.bits := 0.U.asTypeOf(new ExecuteEntry())
  connectQueue(1)
  connectQueue(0)

  // ready logic
  io.in.ready := true.B
  switch(issueStageReg) {
    is(IssueStatus.both) {
      when(
        requireQueue(0) === ExecuteQueueType.none &&
          requireQueue(1) === ExecuteQueueType.none
      ) {
        io.in.ready := true.B
      }.elsewhen(
        requireQueue(0) =/= requireQueue(1) &&
          queueReady(0) &&
          queueReady(1)
      ) { // Parallel
        io.in.ready := true.B
      }.otherwise { // Serial
        io.in.ready := false.B
      }
    }

    is(IssueStatus.first) {
      io.in.ready := queueReady(0)
    }

    is(IssueStatus.second) {
      io.in.ready := queueReady(1)
    }
  }

  // Valid queue logic
  def validQueue(id: Int) = {
    switch(requireQueue(id)) {
      is(ExecuteQueueType.memory) {
        io.memoryEnq.valid := true.B
      }
      is(ExecuteQueueType.alu) {
        io.aluEnq.valid := true.B
      }
      is(ExecuteQueueType.branch) {
        io.branchEnq.valid := true.B
      }
    }
  }

  io.memoryEnq.valid := false.B
  io.aluEnq.valid := false.B
  io.branchEnq.valid := false.B
  switch(issueStageReg) {
    is(IssueStatus.both) {
      when(
        requireQueue(0) === ExecuteQueueType.none &&
          requireQueue(1) === ExecuteQueueType.none
      ) {
        // Do nothing
      }.elsewhen(
        requireQueue(0) =/= requireQueue(1) &&
          queueReady(0) &&
          queueReady(1)
      ) { // Parallel
        when(io.in.valid) {
          validQueue(0)
          validQueue(1)
        }
      }.otherwise { // Serial
        validQueue(0)
      }
    }

    is(IssueStatus.first) {
      when(io.in.valid) {
        validQueue(0)
      }
    }

    is(IssueStatus.second) {
      when(io.in.valid) {
        validQueue(1)
      }
    }
  }

  // Status logic
  switch(issueStageReg) {
    is(IssueStatus.both) {
      when(
        requireQueue(0) === ExecuteQueueType.none &&
          requireQueue(1) === ExecuteQueueType.none
      ) {
        issueStageReg := IssueStatus.both
      }.elsewhen(
        requireQueue(0) =/= requireQueue(1) &&
          queueReady(0) && queueValid(0) &&
          queueReady(1) && queueValid(1)
      ) {
        issueStageReg := IssueStatus.both
      }.elsewhen(queueReady(0) && queueValid(0)) {
        issueStageReg := IssueStatus.second
      }.elsewhen(queueReady(1) && queueValid(1)) {
        issueStageReg := IssueStatus.first
      }
    }
    is(IssueStatus.first) {
      when(queueReady(0) && queueValid(0)) {
        issueStageReg := IssueStatus.both
      }
    }
    is(IssueStatus.second) {
      when(queueReady(1) && queueValid(1)) {
        issueStageReg := IssueStatus.both
      }
    }
  }
}
