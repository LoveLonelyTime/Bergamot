package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.record._

object ALUOperationType extends ChiselEnum {
  val none, add, sub, and, or, xor, sll, srl, sra, slt, sltu = Value
}

class ALU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
  })

}

class ALUDecodeStageEntry extends Bundle {
  val op = ALUOperationType()
  val op1 = DataType.operationType.cloneType
  val op2 = DataType.operationType.cloneType
  val rd = DataType.receiptType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}

class ExecuteResultEntry extends Bundle {
  val result = DataType.operationType.cloneType
  val rd = DataType.receiptType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val vaild = Bool()
}

class ALUDecodeStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ALUDecodeStageEntry())
  })
  private val inReg = Reg(new ExecuteEntry())

  when(io.in.ready && io.in.valid) {
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // Decode logic
  io.out.bits.op := ALUOperationType.none
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
          io.out.bits.op := ALUOperationType.and
        }.otherwise {
          io.out.bits.op := ALUOperationType.sub
        }
      }.elsewhen(inReg.opcode === "b0010011".U) { // I
        io.out.bits.op := ALUOperationType.and
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

  when(inReg.opcode === "b0110011".U) { // R
    io.out.bits.op1 := inReg.rs1.receipt
    io.out.bits.op2 := inReg.rs2.receipt
  }.elsewhen(inReg.opcode === "b0010011".U) { // I
    io.out.bits.op1 := inReg.rs1.receipt
    // Extend
    io.out.bits.op2 := Fill(20, inReg.imm(11)) ## inReg.imm(11, 0)
  }.otherwise {
    io.out.bits.op1 := 0.U
    io.out.bits.op2 := 0.U
  }

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.vaild := inReg.vaild

  io.out.valid := io.in.valid
}

class ALUExecuteStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ALUDecodeStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
  })

  private val inReg = Reg(new ALUDecodeStageEntry())

  io.in.ready := io.out.ready

  when(io.in.ready && io.in.valid) {
    inReg := io.in.bits
  }

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

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.vaild := inReg.vaild

  io.out.valid := io.in.valid
}
