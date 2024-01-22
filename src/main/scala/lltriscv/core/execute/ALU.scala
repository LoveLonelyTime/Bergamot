package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.utils.CoreUtils
import lltriscv.utils.ChiselUtils._
import lltriscv.core.decode.InstructionType

/*
 * ALU (Arithmetic and Logic Unit), which is suitable for integer operations
 *
 * List of supported instructions:
 * - I: lui, auipc, add, sub, sll, slt, sltu, xor, srl, sra, or, and, addi, slti, sltiu, xori, ori, andi, slli, srli, srai, ecall, ebreak, fence
 * - M: mul, mulh, mulhsu, mulhu, div, divu, rem, remu
 * - Zicsr: csrrw, csrrs, csrrc, csrrwi, csrrsi, csrrci
 * - M-Level: mret
 * - S-Level: sret
 * - Zifencei: fence.i
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ALU
  *
  * Execute basic ALU instructions
  *
  * ALUDecodeStage -> ALUExecuteStage
  */
class ALU extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // CSR read interface
    val csr = Flipped(new CSRsReadIO())
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // Recovery interface
    val recover = Input(Bool())
  })
  private val aluDecodeStage = Module(new ALUDecodeStage())
  private val aluExecuteStage = Module(new ALUExecuteStage())

  io.in <> aluDecodeStage.io.in
  aluDecodeStage.io.out <> aluExecuteStage.io.in
  aluExecuteStage.io.out <> io.out

  // Recovery interface
  aluDecodeStage.io.recover := io.recover
  aluExecuteStage.io.recover := io.recover

  // Privilege interface
  aluDecodeStage.io.csr <> io.csr
  aluDecodeStage.io.privilege := io.privilege
  aluExecuteStage.io.privilege := io.privilege
}

/** ALU decode stage
  *
  * Secondary decoding of instructions: extract information and read CSRs
  *
  * Single cycle stage
  */
class ALUDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ALUExecuteStageEntry())
    // CSR read interface
    val csr = Flipped(new CSRsReadIO())
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new ExecuteEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // Decode logic
  // op
  io.out.bits.op := ALUOperationType.undefined

  switch(inReg.opcode(6, 2)) {
    is("b00100".U) { // Basic type I Instructions
      switch(inReg.func3) {
        is("b000".U) {
          io.out.bits.op := ALUOperationType.add
        }
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
        is("b101".U) { // slri / srai
          io.out.bits.op := Mux(inReg.func7(5), ALUOperationType.sra, ALUOperationType.srl)
        }
      }
    }

    is("b01100".U) { // Basic type R Instructions
      when(inReg.func7(0)) { // M Extensions
        switch(inReg.func3) {
          is("b000".U) {
            io.out.bits.op := ALUOperationType.mul
          }
          is("b001".U) {
            io.out.bits.op := ALUOperationType.mulh
          }
          is("b010".U) {
            io.out.bits.op := ALUOperationType.mulhsu
          }
          is("b011".U) {
            io.out.bits.op := ALUOperationType.mulhu
          }
          is("b100".U) {
            io.out.bits.op := ALUOperationType.div
          }
          is("b101".U) {
            io.out.bits.op := ALUOperationType.divu
          }
          is("b110".U) {
            io.out.bits.op := ALUOperationType.rem
          }
          is("b111".U) {
            io.out.bits.op := ALUOperationType.remu
          }
        }
      }.otherwise {
        switch(inReg.func3) {
          is("b000".U) { // add / sub
            io.out.bits.op := Mux(inReg.func7(5), ALUOperationType.sub, ALUOperationType.add)
          }
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
          is("b101".U) { // slr / sra
            io.out.bits.op := Mux(inReg.func7(5), ALUOperationType.sra, ALUOperationType.srl)
          }
        }
      }
    }

    is("b01101".U) { // lui
      io.out.bits.op := ALUOperationType.add
    }
    is("b00101".U) { // auipc
      io.out.bits.op := ALUOperationType.add
    }

    is("b11100".U) { // csr, ecall, ebreak, xret
      switch(inReg.func3) {
        is("b000".U) { // xret, env, ebreak
          io.out.bits.op := Mux(inReg.imm(1), ALUOperationType.xret, ALUOperationType.env)
        }
        is("b001".U) {
          io.out.bits.op := ALUOperationType.csrrw
        }
        is("b010".U) {
          io.out.bits.op := ALUOperationType.csrrs
        }
        is("b011".U) {
          io.out.bits.op := ALUOperationType.csrrc
        }
        is("b101".U) {
          io.out.bits.op := ALUOperationType.csrrw
        }
        is("b110".U) {
          io.out.bits.op := ALUOperationType.csrrs
        }
        is("b111".U) {
          io.out.bits.op := ALUOperationType.csrrc
        }
      }
    }

    is("b00011".U) { // fence, fence.i
      io.out.bits.op := Mux(inReg.func3(0), ALUOperationType.fencei, ALUOperationType.fence)
    }
  }

  // op1 & op2 & csr
  io.out.bits.op1 := 0.U
  io.out.bits.op2 := 0.U
  io.out.bits.csrAddress := 0.U
  io.out.bits.csrError := false.B
  io.csr.address := 0.U

  switch(inReg.instructionType) {
    is(InstructionType.R) { // R
      io.out.bits.op1 := inReg.rs1.receipt
      io.out.bits.op2 := inReg.rs2.receipt
    }

    is(InstructionType.I) { // I
      when(inReg.opcode(6, 2) === "b11100".U && inReg.func3 =/= 0.U) { // CSR I
        // CSR access
        io.out.bits.csrAddress := inReg.imm(11, 0)
        io.csr.address := inReg.imm(11, 0)

        io.out.bits.op1 := io.csr.data
        io.out.bits.op2 := Mux(inReg.func3(2), inReg.zimm, inReg.rs1.receipt)

        io.out.bits.csrError :=
          io.csr.error || // Non-existent CSR
            ((io.out.bits.op2 =/= 0.U || io.out.bits.op === ALUOperationType.csrrw) && inReg.imm(11, 10) === "b11".U) || // Read-only violate
            (io.privilege === PrivilegeType.S && inReg.imm(9, 8) === "b11".U) || (io.privilege === PrivilegeType.U && inReg.imm(9, 8) =/= "b00".U) // Unauthorized access
      }.otherwise { // General I
        io.out.bits.op1 := inReg.rs1.receipt
        io.out.bits.op2 := CoreUtils.signExtended(inReg.imm, 11) // Extend imm
      }
    }

    is(InstructionType.U) { // lui / auipc
      io.out.bits.op1 := Mux(inReg.opcode(5), 0.U, inReg.pc)
      io.out.bits.op2 := inReg.imm // Upper
    }
  }

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid
  io.out.bits.error := inReg.error

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
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
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new ALUExecuteStageEntry().zero)

  io.in.ready := io.out.ready

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Execute logic
  io.out.bits.noResult()
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
    is(ALUOperationType.sll) {
      io.out.bits.result := inReg.op1 << inReg.op2(24, 20)
    }
    is(ALUOperationType.srl) {
      io.out.bits.result := inReg.op1 >> inReg.op2(24, 20)
    }
    is(ALUOperationType.sra) {
      io.out.bits.result := (inReg.op1.asSInt >> inReg.op2(24, 20)).asUInt
    }
    is(ALUOperationType.slt) {
      io.out.bits.result := Mux(inReg.op1.asSInt < inReg.op2.asSInt, 1.U, 0.U)
    }
    is(ALUOperationType.sltu) {
      io.out.bits.result := Mux(inReg.op1 < inReg.op2, 1.U, 0.U)
    }
    is(ALUOperationType.mul) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2.asSInt)(31, 0)
    }
    is(ALUOperationType.mulh) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2.asSInt)(63, 32)
    }
    is(ALUOperationType.mulhsu) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2)(63, 32)
    }
    is(ALUOperationType.mulhu) {
      io.out.bits.result := (inReg.op1 * inReg.op2)(63, 32)
    }
    is(ALUOperationType.div) {
      io.out.bits.result := (inReg.op1.asSInt / inReg.op2.asSInt).asUInt
    }
    is(ALUOperationType.divu) {
      io.out.bits.result := inReg.op1 / inReg.op2
    }
    is(ALUOperationType.rem) {
      io.out.bits.result := (inReg.op1.asSInt % inReg.op2.asSInt).asUInt
    }
    is(ALUOperationType.remu) {
      io.out.bits.result := inReg.op1 % inReg.op2
    }
    is(ALUOperationType.csrrw) {
      io.out.bits.result := inReg.op1
      io.out.bits.resultCSR(inReg.csrAddress, inReg.op2)
    }
    is(ALUOperationType.csrrs) {
      io.out.bits.result := inReg.op1
      // When inReg.op2 === 0, it is a read operation
      when(inReg.op2 =/= 0.U) {
        io.out.bits.resultCSR(inReg.csrAddress, inReg.op1 | inReg.op2)
      }
    }
    is(ALUOperationType.csrrc) {
      io.out.bits.result := inReg.op1
      when(inReg.op2 =/= 0.U) {
        io.out.bits.resultCSR(inReg.csrAddress, inReg.op1 & ~inReg.op2)
      }
    }
    is(ALUOperationType.env) {
      when(inReg.op2(0)) { // ebreak
        io.out.bits.triggerException(ExceptionCode.breakpoint)
      }.otherwise { // ecall
        switch(io.privilege) {
          is(PrivilegeType.M) { io.out.bits.triggerException(ExceptionCode.environmentCallFromMMode) }
          is(PrivilegeType.S) { io.out.bits.triggerException(ExceptionCode.environmentCallFromSMode) }
          is(PrivilegeType.U) { io.out.bits.triggerException(ExceptionCode.environmentCallFromUMode) }
        }
      }
    }
    is(ALUOperationType.xret) {
      when(inReg.op2(9)) { // mret
        when(io.privilege === PrivilegeType.M) { // OK
          io.out.bits.xret := true.B
        }.otherwise { // Unauthorized access
          io.out.bits.triggerException(ExceptionCode.illegalInstruction)
        }
      }.otherwise { // sret
        when(io.privilege === PrivilegeType.S) { // OK
          io.out.bits.xret := true.B
        }.otherwise { // Unauthorized access
          io.out.bits.triggerException(ExceptionCode.illegalInstruction)
        }
      }
    }

    is(ALUOperationType.fence) {
      when(inReg.op2(4) || inReg.op2(6)) { // PO/PW
        io.out.bits.flushDCache := true.B
      }
    }

    is(ALUOperationType.fencei) {
      io.out.bits.flushDCache := true.B
      io.out.bits.flushICache := true.B
    }

    is(ALUOperationType.undefined) {
      io.out.bits.triggerException(ExceptionCode.illegalInstruction)
    }
  }

  // Instruction cache line error
  when(inReg.error === MemoryErrorCode.memoryFault) {
    io.out.bits.triggerException(ExceptionCode.instructionAccessFault)
  }.elsewhen(inReg.error === MemoryErrorCode.pageFault) {
    io.out.bits.triggerException(ExceptionCode.instructionPageFault)
  }

  // CSR Error
  when(inReg.csrError) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction)
  }

  // rd & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.real := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}
