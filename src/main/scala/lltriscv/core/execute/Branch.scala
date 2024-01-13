package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._
import lltriscv.core.record._
import lltriscv.utils.CoreUtils

class Branch extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())

    // Recovery logic
    val recover = Input(Bool())
  })
  private val branchDecodeStage = Module(new BranchDecodeStage())
  private val branchExecuteStage = Module(new BranchExecuteStage())

  io.in <> branchDecodeStage.io.in
  branchDecodeStage.io.out <> branchExecuteStage.io.in
  branchExecuteStage.io.out <> io.out

  // Recovery logic
  branchDecodeStage.io.recover := io.recover
  branchExecuteStage.io.recover := io.recover
}

class BranchDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new BranchExecuteStageEntry())

    // Recovery logic
    val recover = Input(Bool())
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

  // op
  io.out.bits.op := BranchOperationType.undefined
  when(inReg.opcode(2) === 1.U) { // jal & jalr
    io.out.bits.op := BranchOperationType.jal
  }.otherwise {
    switch(inReg.func3) {
      is("b000".U) { io.out.bits.op := BranchOperationType.eq }
      is("b001".U) { io.out.bits.op := BranchOperationType.ne }
      is("b100".U) { io.out.bits.op := BranchOperationType.lt }
      is("b101".U) { io.out.bits.op := BranchOperationType.ge }
      is("b110".U) { io.out.bits.op := BranchOperationType.ltu }
      is("b111".U) { io.out.bits.op := BranchOperationType.geu }
    }
  }

  // op1 & op2
  io.out.bits.op1 := inReg.rs1.receipt
  io.out.bits.op2 := inReg.rs2.receipt

  // add1
  when(inReg.opcode(3, 2) === "b01".U) { // jalr
    io.out.bits.add1 := inReg.rs1.receipt
  }.otherwise { // pc jump
    io.out.bits.add1 := inReg.pc
  }

  // add2
  when(inReg.opcode(3, 2) === "b01".U) { // J
    io.out.bits.add2 := CoreUtils.signExtended(inReg.imm, 20)
  }.elsewhen(inReg.opcode(3, 2) === "b11".U) { // I
    io.out.bits.add2 := CoreUtils.signExtended(inReg.imm, 11)
  }.otherwise { // B
    io.out.bits.add2 := CoreUtils.signExtended(inReg.imm, 12)
  }

  // rd & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

class BranchExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new BranchExecuteStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())

    // Recovery logic
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = Reg(new BranchExecuteStageEntry())

  io.in.ready := io.out.ready

  when(io.out.ready && io.out.valid) { // Stall
    inReg.valid := false.B
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  // Compare logic
  private val geu = WireInit(inReg.op1 > inReg.op2)
  private val eq = WireInit(inReg.op1 === inReg.op2)
  private val ne = WireInit(!eq)
  private val ltu = WireInit(!eq && !geu)
  private val ge = WireInit(false.B)
  private val sign = inReg.op1(31) ## inReg.op2(31)
  switch(sign) {
    is("b00".U) { ge := geu }
    is("b01".U) { ge := true.B }
    is("b10".U) { ge := false.B }
    is("b11".U) { ge := geu }
  }
  private val lt = WireInit(!eq && !ge)

  private val addPC = WireInit(inReg.add1 + inReg.add2)

  // Real PC
  io.out.bits.real := inReg.next
  switch(inReg.op) {
    is(BranchOperationType.eq) {
      io.out.bits.real := Mux(eq, addPC, inReg.next)
    }
    is(BranchOperationType.ne) {
      io.out.bits.real := Mux(ne, addPC, inReg.next)
    }
    is(BranchOperationType.ge) {
      io.out.bits.real := Mux(ge, addPC, inReg.next)
    }
    is(BranchOperationType.lt) {
      io.out.bits.real := Mux(lt, addPC, inReg.next)
    }
    is(BranchOperationType.geu) {
      io.out.bits.real := Mux(geu, addPC, inReg.next)
    }
    is(BranchOperationType.ltu) {
      io.out.bits.real := Mux(geu, addPC, inReg.next)
    }
    is(BranchOperationType.jal) {
      io.out.bits.real := addPC & ~1.U(32.W) // Reset LSB
    }
  }

  // rd & result & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.result := inReg.next
  io.out.bits.pc := inReg.pc
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}
