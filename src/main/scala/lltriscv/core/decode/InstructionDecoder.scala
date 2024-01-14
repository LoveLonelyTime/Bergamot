package lltriscv.core.decode

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.core.execute._
import lltriscv.utils.CoreUtils
import lltriscv.core.broadcast.DataBroadcastIO

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
  require(executeQueueWidth > 0, "Execute queue depth must be greater than 0")
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interface
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
    // ROB table write interface
    val tableWrite = new ROBTableWriteIO()

    // Recovery interface
    val recover = Input(Bool())
  })

  // Pipeline stages
  private val decodeStage = Module(new DecodeStage())
  private val registerMappingStage = Module(new RegisterMappingStage())
  private val issueStage = Module(new IssueStage(executeQueueWidth))

  io.in <> decodeStage.io.in
  decodeStage.io.out <> registerMappingStage.io.in
  registerMappingStage.io.out <> issueStage.io.in
  issueStage.io.enqs <> io.enqs

  registerMappingStage.io.mapping <> io.mapping
  registerMappingStage.io.tableWrite <> io.tableWrite

  issueStage.io.broadcast <> io.broadcast

  decodeStage.io.recover := io.recover
  registerMappingStage.io.recover := io.recover
  issueStage.io.recover := io.recover
}

/** Decode stage
  *
  * Split according to instruction format
  *
  * Single cycle stage
  */
class DecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
    val out = DecoupledIO(Vec(2, new RegisterMappingStageEntry()))

    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = Reg(Vec(2, new DecodeStageEntry()))

  io.in.ready := io.out.ready
  when(io.out.valid && io.out.ready) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.valid && io.in.ready) { // Sample
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

    // Execute queue arbitration
    when(
      io.out.bits(i).opcode(6, 2) === "b11011".U || // jal
        io.out.bits(i).opcode(6, 2) === "b11001".U || // jalr
        instructionType === InstructionType.B
    ) {
      io.out.bits(i).executeQueue := ExecuteQueueType.branch
    }.elsewhen(
      io.out.bits(i).opcode(6, 2) === "b00000".U || // load
        instructionType === InstructionType.S
    ) {
      io.out.bits(i).executeQueue := ExecuteQueueType.memory
    }.otherwise {
      io.out.bits(i).executeQueue := ExecuteQueueType.alu
    }

    // pc & valid
    io.out.bits(i).pc := inReg(i).pc
    io.out.bits(i).next := inReg(i).next
    io.out.bits(i).spec := inReg(i).spec
    io.out.bits(i).valid := inReg(i).valid
  }

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}

/** Register mapping stage
  *
  * Allocate and get renaming registers and ROB
  *
  * Multicycle stage
  */
class RegisterMappingStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec(2, new RegisterMappingStageEntry())))
    val out = DecoupledIO(Vec(2, new IssueStageEntry()))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // ROBTable interface
    val tableWrite = new ROBTableWriteIO()

    // Recovery logic
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = Reg(Vec(2, new RegisterMappingStageEntry()))

  // Waiting for mapping
  io.in.ready := io.mapping.ready && io.out.ready
  when(io.out.valid && io.out.ready) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.valid && io.in.ready) { // Sample
    inReg := io.in.bits
  }

  // Mapping logic
  io.mapping.valid := io.out.ready
  for (i <- 0 until 2) {
    // rs1
    io.mapping.regGroup(i).rs1 := Mux(
      inReg(i).valid,
      inReg(i).rs1,
      0.U // Bypass
    )
    // rs2
    io.mapping.regGroup(i).rs2 := Mux(
      inReg(i).valid,
      inReg(i).rs2,
      0.U // Bypass
    )
    // rd
    io.mapping.regGroup(i).rd := Mux(
      inReg(i).valid,
      inReg(i).rd,
      0.U // Bypass
    )
  }

  // Output logic
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
    io.out.bits(i).next := inReg(i).next
    io.out.bits(i).valid := inReg(i).valid

    // Write ROB table
    io.tableWrite.entries(i).id := io.mapping.mappingGroup(i).rd
    io.tableWrite.entries(i).pc := inReg(i).pc
    io.tableWrite.entries(i).rd := inReg(i).rd
    io.tableWrite.entries(i).spec := inReg(i).spec
    io.tableWrite.entries(i).valid := inReg(i).valid
  }

  // Synchronize with mapping
  io.tableWrite.wen := io.mapping.valid && io.mapping.ready

  // Waiting for mapping
  io.out.valid := io.mapping.ready

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}

/** Issue stage
  *
  * Instruction queue arbitration
  *
  * Select executable instructions to the instruction queue. When the execution
  * queue types of two instructions are different, two instructions can be
  * issued at once. If two instructions are the same type, will be issued in two
  * cycles.
  *
  * Multicycle stage
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class IssueStage(executeQueueWidth: Int) extends Module {
  require(executeQueueWidth > 0, "Execute queue depth must be greater than 0")

  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec(2, new IssueStageEntry())))
    // Broadcast
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interfaces
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())

    // Recovery logic
    val recover = Input(Bool())
  })

  private val inReg = Reg(Vec(2, new IssueStageEntry()))

  // Broadcast logic
  for (
    i <- 0 until 2;
    j <- 0 until 2
  ) {
    CoreUtils.matchBroadcast(inReg(i).rs1, io.broadcast.entries(j))
    CoreUtils.matchBroadcast(inReg(i).rs2, io.broadcast.entries(j))
  }

  // Check queue grants
  private val grants = VecInit(
    VecInit.fill(executeQueueWidth)(false.B), // Instruction 0 grant
    VecInit.fill(executeQueueWidth)(false.B) // Instruction 1 grant
  )
  private val queueReady = VecInit(true.B, true.B)

  // Grant logic
  io.enqs.foreach(_.enq.valid := false.B)

  // Instruction 0 grant
  for (i <- 0 until executeQueueWidth) {
    grants(0)(i) := inReg(0).executeQueue === io.enqs(i).queueType &&
      inReg(0).valid
    when(grants(0)(i)) {
      queueReady(0) := io.enqs(i).enq.ready
      io.enqs(i).enq.valid := true.B
    }
  }

  // Instruction 1 grant
  for (i <- 0 until executeQueueWidth) {
    grants(1)(i) := inReg(1).executeQueue === io.enqs(i).queueType &&
      inReg(1).valid &&
      !grants(0)(i) // Priority

    when(grants(1)(i)) {
      queueReady(1) := io.enqs(i).enq.ready
      io.enqs(i).enq.valid := true.B
    }.elsewhen(
      inReg(1).executeQueue === io.enqs(i).queueType &&
        inReg(1).valid &&
        grants(0)(i)
    ) { // Priority
      queueReady(1) := false.B
    }
  }

  for (i <- 0 until executeQueueWidth) {
    io.enqs(i).enq.bits := 0.U.asTypeOf(new ExecuteEntry())
  }

  for (
    i <- 0 until 2;
    j <- 0 until executeQueueWidth
  ) {
    when(grants(i)(j)) {
      io.enqs(j).enq.bits.opcode := inReg(i).opcode
      io.enqs(j).enq.bits.instructionType := inReg(i).instructionType
      io.enqs(j).enq.bits.executeQueue := inReg(i).executeQueue

      // Broadcast bypass
      // rs1
      for (k <- 0 until 2)
        io.enqs(j).enq.bits.rs1 := CoreUtils.bypassBroadcast(
          inReg(i).rs1,
          io.broadcast.entries(k)
        )
      // rs2
      for (k <- 0 until 2)
        io.enqs(j).enq.bits.rs2 := CoreUtils.bypassBroadcast(
          inReg(i).rs2,
          io.broadcast.entries(k)
        )

      io.enqs(j).enq.bits.rd := inReg(i).rd
      io.enqs(j).enq.bits.func3 := inReg(i).func3
      io.enqs(j).enq.bits.func7 := inReg(i).func7
      io.enqs(j).enq.bits.imm := inReg(i).imm
      io.enqs(j).enq.bits.pc := inReg(i).pc
      io.enqs(j).enq.bits.next := inReg(i).next
      io.enqs(j).enq.bits.valid := inReg(i).valid
    }
  }

  for (i <- 0 until 2) { // Stall
    when(queueReady(i)) {
      inReg(i).valid := false.B
    }
  }

  // Pipeline logic
  io.in.ready := queueReady(0) && queueReady(1)
  when(io.in.valid && io.in.ready) {
    inReg := io.in.bits
  }

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}
