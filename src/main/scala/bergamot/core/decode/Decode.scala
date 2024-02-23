package bergamot.core.decode

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastIO
import bergamot.core.record.RegisterMappingIO
import bergamot.core.record.ROBTableWriteIO
import bergamot.core.execute.MemoryErrorCode
import bergamot.core.execute.ExecuteQueueType
import bergamot.core.execute.ExecuteQueueEnqueueIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Instruction decode
 *
 * The decoder decodes instructions and dispatches them according to the type of execution queue.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Decode components
  *
  * The instruction decoding stage is divided into three cycles: DecodeStage -> RegisterMappingStage -> IssueStage.
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class Decode(executeQueueWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec2(new DecodeStageEntry())))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interface
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
    // ROB table write interface
    val robTableWrite = new ROBTableWriteIO()
    // Recovery interface
    val recover = Input(Bool())

    val hit = Input(Bool())
  })

  // Pipeline stages
  private val decodeStage = Module(new DecodeStage())
  private val registerMappingStage = Module(new RegisterMappingStage())
  private val issueStage = Module(new IssueStage(executeQueueWidth))

  decodeStage.io.hit := io.hit
  registerMappingStage.io.hit := io.hit

  io.in <> decodeStage.io.in
  decodeStage.io.out <> registerMappingStage.io.in
  registerMappingStage.io.out <> issueStage.io.in
  issueStage.io.enqs <> io.enqs

  registerMappingStage.io.mapping <> io.mapping
  registerMappingStage.io.robTableWrite <> io.robTableWrite

  issueStage.io.broadcast <> io.broadcast

  decodeStage.io.recover := io.recover
  registerMappingStage.io.recover := io.recover
  issueStage.io.recover := io.recover
}

/** Decode stage
  *
  * Decode according to instruction format
  *
  * Single cycle stage
  */
class DecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec2(new DecodeStageEntry())))
    val out = DecoupledIO(Vec2(new RegisterMappingStageEntry()))
    // Recovery interface
    val recover = Input(Bool())

    val hit = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(Vec2(new DecodeStageEntry()).zero)

  io.in.ready := io.out.ready
  when(io.out.fire) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Decode logic
  inReg.zip(io.out.bits).foreach { case (in, out) =>
    // opcode
    out.opcode := in.instruction(6, 0)

    // Instruction Type
    out.instructionType := WireInit(InstructionType.UNK)
    switch(in.instruction(6, 2)) {
      // I: lui
      is("b01101".U) {
        out.instructionType := InstructionType.U
      }
      // I: auipc
      is("b00101".U) {
        out.instructionType := InstructionType.U
      }
      // I: beq, bne, blt, bge, bltu, bgeu
      is("b11000".U) {
        out.instructionType := InstructionType.B
      }
      // I: sb, sh, sw
      is("b01000".U) {
        out.instructionType := InstructionType.S
      }
      // I: add, sub, sll, slt, sltu, xor, srl, sra, or, and
      // M: mul, mulh, mulhsu, mulhu, div, divu, rem, remu
      is("b01100".U) {
        out.instructionType := InstructionType.R
      }
      // I: jal
      is("b11011".U) {
        out.instructionType := InstructionType.J
      }
      // I: lb, lh, lw, lbu, lhu
      is("b00000".U) {
        out.instructionType := InstructionType.I
      }
      // I: addi, slti, sltiu, xori, ori, andi, slli, srli, srai
      is("b00100".U) {
        out.instructionType := InstructionType.I
      }
      // I: jalr
      is("b11001".U) {
        out.instructionType := InstructionType.I
      }
      // Zicsr: csrrw, csrrs, csrrc, csrrwi, csrrsi, csrrci
      // I: ecall, ebreak, wfi, sfence.vma
      // M-Level: mret
      // S-Level: sret
      is("b11100".U) {
        out.instructionType := InstructionType.I
      }
      // I: fence
      // Zifencei: fence.i
      is("b00011".U) {
        out.instructionType := InstructionType.I
      }
      // A: lr, sc, amoswap, amoadd, amoxor, amoand, amoor, amomin, amomax, amominu, amomaxu
      is("b01011".U) {
        out.instructionType := InstructionType.R
      }
    }

    when(in.instruction(1, 0) =/= "b11".U) { // Identification code detection
      out.instructionType := InstructionType.UNK
    }

    // rs1Tozimm (csrrwi, csrrsi, csrrci)
    // zimm can not occupy the position of rs1
    val rs1Tozimm = in.instruction(6, 2) === "b11100".U && in.instruction(14)

    // rd: R/I/U/J
    out.rd := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.I, InstructionType.U, InstructionType.J)) {
      out.rd := in.instruction(11, 7)
    }

    // rs1(zimm): R/I/S/B
    out.zimm := 0.U
    out.rs1 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.I, InstructionType.S, InstructionType.B)) {
      val rs1 = in.instruction(19, 15)
      when(rs1Tozimm) {
        out.zimm := rs1
      }.otherwise {
        out.rs1 := rs1
      }
    }

    // rs2: R/S/B
    out.rs2 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.S, InstructionType.B)) {
      out.rs2 := in.instruction(24, 20)
    }

    // func3: R/I/S/B
    out.func3 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.I, InstructionType.S, InstructionType.B)) {
      out.func3 := in.instruction(14, 12)
    }

    // func7: R/I (srli,srai)
    out.func7 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.I)) {
      out.func7 := in.instruction(31, 25)
    }

    // imm decode table
    out.imm := MuxLookup(out.instructionType, 0.U)(
      Seq(
        InstructionType.I -> 0.U(20.W) ## in.instruction(31, 20),
        InstructionType.S -> 0.U(20.W) ## in.instruction(31, 25) ## in.instruction(11, 7),
        InstructionType.B -> 0.U(19.W) ## in.instruction(31) ## in.instruction(7) ## in.instruction(30, 25) ## in.instruction(11, 8) ## 0.U,
        InstructionType.U -> in.instruction(31, 12) ## 0.U(12.W),
        InstructionType.J -> 0.U(11.W) ## in.instruction(31) ## in.instruction(19, 12) ## in.instruction(20) ## in.instruction(30, 21) ## 0.U
      )
    )

    // Execute queue arbitration table
    out.executeQueue := MuxCase(
      ExecuteQueueType.alu, // Default ALU
      Seq(
        (in.error =/= MemoryErrorCode.none || out.instructionType === InstructionType.UNK) -> ExecuteQueueType.alu, // Exception handler
        ((out.opcode(6, 2) in ("b11011".U, "b11001".U)) || out.instructionType === InstructionType.B) -> ExecuteQueueType.branch, // jal, jalr, branch
        ((out.opcode(6, 2) in ("b00000".U, "b01011".U)) || out.instructionType === InstructionType.S) -> ExecuteQueueType.memory // load, store, lr, sc, amo
      )
    )

    out.pc := in.pc
    out.next := in.next
    out.spec := in.spec
    out.error := in.error
    out.valid := in.valid
  }

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }

  when(io.hit) {
    printf(
      "DecodeStage[>]I0PC=%x,I0=opcode:%x func7:%x func3:%x imm:%x rs1:%x rs2:%x rd:%x,I0T=%x,I0E=%x,I1PC=%x,I1=opcode:%x func7:%x func3:%x imm:%x rs1:%x rs2:%x rd:%x,I1T=%x,I1E=%x\n",
      io.out.bits(0).pc,
      io.out.bits(0).opcode,
      io.out.bits(0).func7,
      io.out.bits(0).func3,
      io.out.bits(0).imm,
      io.out.bits(0).rs1,
      io.out.bits(0).rs2,
      io.out.bits(0).rd,
      io.out.bits(0).instructionType.asUInt,
      io.out.bits(0).executeQueue.asUInt,
      io.out.bits(1).pc,
      io.out.bits(1).opcode,
      io.out.bits(1).func7,
      io.out.bits(1).func3,
      io.out.bits(1).imm,
      io.out.bits(1).rs1,
      io.out.bits(1).rs2,
      io.out.bits(1).rd,
      io.out.bits(1).instructionType.asUInt,
      io.out.bits(1).executeQueue.asUInt
    )
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
    val in = Flipped(DecoupledIO(Vec2(new RegisterMappingStageEntry())))
    val out = DecoupledIO(Vec2(new IssueStageEntry()))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // ROBTable interface
    val robTableWrite = new ROBTableWriteIO()
    // Recovery logic
    val recover = Input(Bool())

    val hit = Input(Bool())
  })

  // Pipeline logic
  private val inReg = RegInit(Vec2(new RegisterMappingStageEntry()).zero)

  // Waiting for mapping
  io.in.ready := io.mapping.ready && io.out.ready
  when(io.out.fire) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.mapping <> new RegisterMappingIO().zero

  // Mapping logic
  io.mapping.valid := io.out.ready
  inReg.zip(io.mapping.regGroup).foreach { case (in, regGroup) =>
    when(in.valid && in.error === MemoryErrorCode.none) {
      regGroup.rs1 := in.rs1
      regGroup.rs2 := in.rs2
      regGroup.rd := in.rd
    }
  }

  // Output logic
  io.out.bits
    .zip(io.robTableWrite.entries)
    .zip(inReg.zip(io.mapping.mappingGroup))
    .foreach { case ((out, robTableWriteEntry), (in, mappingGroup)) =>
      out.opcode := in.opcode
      out.instructionType := in.instructionType
      out.executeQueue := in.executeQueue

      // Mapping result
      out.rs1 := mappingGroup.rs1
      out.rs2 := mappingGroup.rs2
      out.rd := mappingGroup.rd

      out.func3 := in.func3
      out.func7 := in.func7
      out.imm := in.imm
      out.zimm := in.zimm

      out.pc := in.pc
      out.next := in.next
      out.error := in.error
      out.valid := in.valid

      // Write ROB table
      robTableWriteEntry.id := mappingGroup.rd
      robTableWriteEntry.pc := in.pc
      robTableWriteEntry.rd := in.rd
      robTableWriteEntry.spec := in.spec
      robTableWriteEntry.valid := in.valid
    }

  // Synchronize with mapping
  io.robTableWrite.wen := io.mapping.valid && io.mapping.ready

  // Waiting for mapping
  io.out.valid := io.mapping.ready

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }

  // Print
  when(io.hit) {
    printf(
      "RegisterMappingStage[>]I0PC=%x,I0=r1:%x -> %x(Pending:%x) r2:%x -> %x(Pending:%x) rd:%x -> %x,I1PC=%x,I1=r1:%x -> %x(Pending:%x) r2:%x -> %x(Pending:%x) rd:%x -> %x\n",
      inReg(0).pc,
      inReg(0).rs1,
      io.out.bits(0).rs1.receipt,
      io.out.bits(0).rs1.pending,
      inReg(0).rs2,
      io.out.bits(0).rs2.receipt,
      io.out.bits(0).rs2.pending,
      inReg(0).rd,
      io.out.bits(0).rd,
      inReg(1).pc,
      inReg(1).rs1,
      io.out.bits(1).rs1.receipt,
      io.out.bits(1).rs1.pending,
      inReg(1).rs2,
      io.out.bits(1).rs2.receipt,
      io.out.bits(1).rs2.pending,
      inReg(1).rd,
      io.out.bits(1).rd
    )
  }

}

/** Issue stage
  *
  * Instruction queue arbitration
  *
  * Select executable instructions to the instruction queue.
  *
  * When the execution queue types of two instructions are different, two instructions can be issued at once.
  *
  * If two instructions are the same type, will be issued in two cycles.
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
    val in = Flipped(DecoupledIO(Vec2(new IssueStageEntry())))
    // Broadcast
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interfaces
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
    // Recovery logic
    val recover = Input(Bool())
  })

  private val inReg = RegInit(Vec2(new IssueStageEntry()).zero)

  // Check queue grants
  private val grants = VecInit2(VecInit.fill(executeQueueWidth)(false.B))
  private val queueReady = VecInit2(true.B)

  // Broadcast logic
  for (
    in <- inReg;
    broadcast <- io.broadcast.entries
  ) {
    matchBroadcast(in.rs1, in.rs1, broadcast)
    matchBroadcast(in.rs2, in.rs2, broadcast)
  }

  // Pipeline logic
  queueReady.zip(inReg).foreach { case (ready, in) =>
    when(ready) { // Stall
      in.valid := false.B
    }
  }

  io.in.ready := queueReady(0) && queueReady(1)
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Connect entry to queue
  private def connectQueue(entry: IssueStageEntry, queue: ExecuteQueueEnqueueIO) = {
    queue.enq.bits.opcode := entry.opcode
    queue.enq.bits.instructionType := entry.instructionType

    // Broadcast bypass
    // rs1
    queue.enq.bits.rs1 := entry.rs1
    io.broadcast.entries.foreach { broadcast =>
      matchBroadcast(queue.enq.bits.rs1, entry.rs1, broadcast)
    }
    // rs2
    queue.enq.bits.rs2 := entry.rs2
    io.broadcast.entries.foreach { broadcast =>
      matchBroadcast(queue.enq.bits.rs2, entry.rs2, broadcast)
    }

    queue.enq.bits.rd := entry.rd
    queue.enq.bits.func3 := entry.func3
    queue.enq.bits.func7 := entry.func7
    queue.enq.bits.imm := entry.imm
    queue.enq.bits.zimm := entry.zimm
    queue.enq.bits.pc := entry.pc
    queue.enq.bits.next := entry.next
    queue.enq.bits.error := entry.error
    queue.enq.bits.valid := entry.valid
  }

  // Grant logic
  io.enqs <> Vec(executeQueueWidth, new ExecuteQueueEnqueueIO()).zero

  // Instruction 0 grant
  for (i <- 0 until executeQueueWidth) {
    grants(0)(i) := inReg(0).executeQueue === io.enqs(i).queueType && inReg(0).valid
    when(grants(0)(i)) {
      queueReady(0) := io.enqs(i).enq.ready
      connectQueue(inReg(0), io.enqs(i))
      io.enqs(i).enq.valid := true.B
    }
  }

  // Instruction 1 grant
  for (i <- 0 until executeQueueWidth) {
    grants(1)(i) := inReg(1).executeQueue === io.enqs(i).queueType && inReg(1).valid && !grants(0)(i) // Priority

    when(grants(1)(i)) {
      queueReady(1) := io.enqs(i).enq.ready
      connectQueue(inReg(1), io.enqs(i))
      io.enqs(i).enq.valid := true.B
    }.elsewhen(inReg(1).executeQueue === io.enqs(i).queueType && inReg(1).valid && grants(0)(i)) { // Conflict
      queueReady(1) := false.B
    }
  }

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}
