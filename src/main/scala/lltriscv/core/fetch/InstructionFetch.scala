package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry
import lltriscv.utils.ChiselUtils._

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
    val pc = Output(DataType.address)
    // Prediction failure and correction PC
    val correctPC = Input(DataType.address)
    // Recovery interface
    val recover = Input(Bool())
  })
  // PC register
  private val pcReg = RegInit(DataType.address.zeroAsUInt)
  io.pc := pcReg

  // Pipeline logic
  private val inReg = RegInit(Vec(2, new PCVerifyStageEntry()).zero)
  when(io.out.fire) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.fire) { // Sample
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

  // PC next logic
  when(verify && inReg(0).valid) {
    val nextPC = Wire(DataType.address)
    when(inReg(1).valid && inReg(0).spec === inReg(1).pc) { // 0 -> 1 -> spec
      nextPC := inReg(1).spec
    }.otherwise { // 0 -> spec, 1 is masked
      nextPC := inReg(0).spec
      io.out.bits(1).valid := false.B
    }
    // Prefetch
    io.pc := nextPC

    when(io.out.fire) {
      pcReg := nextPC
    }
  }

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
    pcReg := io.correctPC
  }
}
