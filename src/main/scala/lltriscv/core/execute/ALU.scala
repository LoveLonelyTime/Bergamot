package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.utils.CoreUtils

/*
 * ALU (Arithmetic and Logic Unit), which is suitable for integer operations (RV32I)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ALU execute queue
  *
  * ALUDecodeStage -> ALUExecuteStage
  */
class ALU extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
  })
  private val aluDecodeStage = Module(new ALUDecodeStage())
  private val aluExecuteStage = Module(new ALUExecuteStage())

  io.in <> aluDecodeStage.io.in
  aluDecodeStage.io.out <> aluExecuteStage.io.in
  aluExecuteStage.io.out <> io.out
}

/** ALU decode stage
  *
  * Secondary decoding of instructions: extract information (op1, op2, op),
  * extend immediate
  *
  * Single cycle stage
  */
class ALUDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ALUExecuteStageEntry())
  })
  // Pipeline logic
  private val inReg = Reg(new ExecuteEntry())

  when(io.out.ready && io.out.valid) { // Stall
    inReg.valid := false.B
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // TODO: optimize
  // Decode logic

  // op
  io.out.bits.op := ALUOperationType.undefined
  switch(inReg.func3) {
    is("b100".U) {
      io.out.bits.op := ALUOperationType.xor
    }
    is("b110".U) {
      io.out.bits.op := ALUOperationType.or
    }
    is("b111".U) {
      io.out.bits.op := ALUOperationType.and
    }
    is("b001".U) {
      io.out.bits.op := ALUOperationType.sll
    }
    is("b010".U) {
      io.out.bits.op := ALUOperationType.slt
    }
    is("b011".U) {
      io.out.bits.op := ALUOperationType.sltu
    }

    is("b000".U) {
      when(inReg.opcode === "b0110011".U) { // R
        when(inReg.func7(5) === 0.U) {
          io.out.bits.op := ALUOperationType.add
        }.otherwise {
          io.out.bits.op := ALUOperationType.sub
        }
      }.elsewhen(inReg.opcode === "b0010011".U) { // I
        io.out.bits.op := ALUOperationType.add
      }
    }

    is("b101".U) {
      when(inReg.func7(5) === 0.U) {
        io.out.bits.op := ALUOperationType.srl
      }.otherwise {
        io.out.bits.op := ALUOperationType.sra
      }
    }
  }

  // op1 & op2
  io.out.bits.op1 := 0.U
  io.out.bits.op2 := 0.U
  when(inReg.opcode === "b0110011".U) { // R
    io.out.bits.op1 := inReg.rs1.receipt
    io.out.bits.op2 := inReg.rs2.receipt
  }.elsewhen(inReg.opcode === "b0010011".U) { // I
    io.out.bits.op1 := inReg.rs1.receipt
    // Extend
    io.out.bits.op2 := CoreUtils.signExtended(inReg.imm, 11)
  }

  // rd & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait
}

/** ALU execute stage
  *
  * Execute ALU operation
  *
  * Single cycle stage
  */
class ALUExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ALUExecuteStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
  })
  // Pipeline logic
  private val inReg = Reg(new ALUExecuteStageEntry())

  io.in.ready := io.out.ready

  when(io.out.ready && io.out.valid) { // Stall
    inReg.valid := false.B
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  // TODO: complete
  // Execute
  io.out.bits.result := 0.U
  switch(inReg.op) {
    is(ALUOperationType.add) {
      io.out.bits.result := inReg.op1 + inReg.op2
    }
    is(ALUOperationType.sub) {
      io.out.bits.result := inReg.op1 - inReg.op2
    }
    is(ALUOperationType.and) {
      io.out.bits.result := inReg.op1 & inReg.op2
    }
    is(ALUOperationType.or) {
      io.out.bits.result := inReg.op1 | inReg.op2
    }
    is(ALUOperationType.xor) {
      io.out.bits.result := inReg.op1 ^ inReg.op2
    }
  }

  // rd & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.valid := inReg.valid
  io.out.bits.real := inReg.next

  io.out.valid := true.B // No wait
}
