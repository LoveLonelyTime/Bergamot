package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core.decode.InstructionType
import lltriscv.utils.CoreUtils
import lltriscv.core.DataType
import lltriscv.core.record.TLBVAddressEntry
import lltriscv.core.record.TLBPAddressEntry

class MemoryDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new MemoryExecuteStageEntry())

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

  // Decode logic

  // op
  io.out.bits.op := MemoryOperationType.undefined
  switch(inReg.instructionType) {
    is(InstructionType.I) {
      switch(inReg.func3) {
        is("b000".U) { io.out.bits.op := MemoryOperationType.lb }
        is("b001".U) { io.out.bits.op := MemoryOperationType.lh }
        is("b010".U) { io.out.bits.op := MemoryOperationType.lw }
        is("b100".U) { io.out.bits.op := MemoryOperationType.lbu }
        is("b101".U) { io.out.bits.op := MemoryOperationType.lhu }
      }
    }
    is(InstructionType.S) {
      switch(inReg.func3) {
        is("b000".U) { io.out.bits.op := MemoryOperationType.sb }
        is("b001".U) { io.out.bits.op := MemoryOperationType.sh }
        is("b010".U) { io.out.bits.op := MemoryOperationType.sw }
      }
    }
  }

  // add1 & add2
  io.out.bits.add1 := inReg.rs1
  io.out.bits.add2 := CoreUtils.signExtended(inReg.imm, 11)

  // op1: the data stored
  io.out.bits.op1 := Mux(
    inReg.instructionType === InstructionType.S,
    inReg.rs2,
    0.U
  )

  // Reserved
  io.out.bits.op2 := 0.U

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

class MemoryExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new MemoryExecuteStageEntry()))
    val out = DecoupledIO(new MemoryTLBStageEntry())

    // Recovery logic
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = Reg(new MemoryExecuteStageEntry())

  when(io.out.ready && io.out.valid) { // Stall
    inReg.valid := false.B
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  // Execute logic
  io.out.bits.op := inReg.op
  io.out.bits.vaddress := inReg.add1 + inReg.add2 // Address addition

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

class MemoryTLBStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new MemoryTLBStageEntry()))
    val out = DecoupledIO(new MemoryReadStageEntry())

    // DTLB interface
    val vaddress = DecoupledIO(new TLBVAddressEntry())
    val paddress = Flipped(DecoupledIO(new TLBPAddressEntry()))

    // Recovery interface
    val recover = Input(Bool())
  })
  private val statusReg = Reg(Status.idle)
  object Status extends ChiselEnum {
    val idle, request, response = Value
  }

  // Pipeline logic
  private val inReg = Reg(new MemoryTLBStageEntry())

  when(io.out.ready && io.out.valid) { // Stall
    inReg.valid := false.B
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits

    when(io.in.bits.valid) {
      statusReg := Status.request
    }
  }

  io.in.ready := statusReg === Status.idle // Idle

  io.vaddress.valid := false.B
  io.vaddress.bits.address := 0.U
  when(statusReg === Status.request) {
    io.vaddress.valid := true.B
    io.vaddress.bits.address := inReg.vaddress

    when(io.vaddress.valid && io.vaddress.ready) {
      statusReg := Status.response
    }
  }

  when(statusReg === Status.response) {
    when(io.paddress.valid && io.paddress.ready) {

      statusReg := Status.idle
    }
  }
}

class MemoryReadStage extends Module {}
