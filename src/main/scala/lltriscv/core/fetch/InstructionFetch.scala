package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry

/*
 * Instruction fetch
 *
 * Instruction fetch is located behind the instruction cache and is mainly used to control PC logic, instruction validation, and branch prediction
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** PC verify stage
  *
  * Control the PC jump logic and verify the fetch instructions
  */
class PCVerifyStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec(2, new PCVerifyStageEntry())))
    val out = DecoupledIO(Vec(2, new DecodeStageEntry()))
    // Current PC
    val pc = Output(DataType.pc)
    // Prediction failure and correction PC
    val correctPC = Input(DataType.pc)
    // Recovery interface
    val recover = Input(Bool())
  })
  // PC register
  private val pcReg = Reg(DataType.pc)
  io.pc := pcReg

  // Pipeline logic
  private val inReg = Reg(Vec(2, new PCVerifyStageEntry()))
  when(io.out.ready && io.out.valid) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.ready && io.in.valid) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // When the PC of instruction(0) is equal to pcReg, pass verification
  private val verify = pcReg === inReg(0).pc

  for (i <- 0 until 2) {
    io.out.bits(i).instruction := inReg(i).instruction
    io.out.bits(i).pc := inReg(i).pc
    io.out.bits(i).spec := inReg(i).spec
    io.out.bits(i).next := inReg(i).next
    // PC Verify
    io.out.bits(i).valid := Mux(verify, inReg(i).valid, false.B)
  }

  io.out.valid := true.B // No wait

  // PC logic
  when(verify && inReg(0).valid) {
    when(inReg(1).valid && inReg(0).spec === inReg(1).pc) { // 0 -> 1 -> spec
      pcReg := inReg(1).spec
    }.otherwise { // 0 -> spec, 1 is masked
      pcReg := inReg(0).spec
      io.out.bits(1).valid := false.B
    }
  }

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
    pcReg := io.correctPC
  }
}
