package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry

class PCVerifyStageEntry extends Bundle {
  // Instruction
  val instruction = DataType.instructionType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Speculative PC
  val spec = DataType.pcType.cloneType
  // Next PC
  val next = DataType.pcType.cloneType
  // Validity
  val valid = Bool()
}

class PCVerifyStage extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new PCVerifyStageEntry())))
    val out = DecoupledIO(Vec(2, new DecodeStageEntry()))
    val pc = Output(DataType.pcType.cloneType)

    val correctPC = Input(DataType.pcType.cloneType)
    val recover = Input(Bool())
  })

  private val pcReg = Reg(DataType.pcType.cloneType)
  io.pc := pcReg

  private val inReg = Reg(Vec(2, new PCVerifyStageEntry()))
  when(io.out.ready && io.out.valid) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  private val verify = pcReg === inReg(0).pc

  for (i <- 0 until 2) {
    io.out.bits(i).instruction := inReg(i).instruction
    io.out.bits(i).pc := inReg(i).pc
    io.out.bits(i).spec := inReg(i).spec
    io.out.bits(i).next := inReg(i).next
    // PC Verify, just 0
    io.out.bits(i).valid := Mux(verify, inReg(i).valid, false.B)
  }

  io.out.valid := true.B // No wait

  // PC logic
  when(verify && inReg(0).valid) {
    when(inReg(1).valid && inReg(0).spec === inReg(1).pc) { // 1 -> 2 -> spec
      pcReg := inReg(1).spec
    }.otherwise { // 1 is masked
      pcReg := inReg(0).spec
      io.out.bits(1).valid := false.B
    }
  }

  // Recovery logic
  when(io.recover) {
    pcReg := io.correctPC
  }
}
