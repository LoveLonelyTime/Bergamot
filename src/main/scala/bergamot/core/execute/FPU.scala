package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._
import bergamot.core.fpu.FPUnbox
import bergamot.core.fpu.IEEESpec32
import bergamot.utils.CoreUtils
import bergamot.core.CoreConstant
import bergamot.core.record.ExceptionCode
import bergamot.core.fpu.IEEESpec64
import bergamot.core.fpu.FP
import bergamot.core.fpu.FPClassCode
import bergamot.core.fpu.FPMul

class FPU extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Recovery interface
    val recover = Input(Bool())
  })

  private val fpuDecodeStage = Module(new FPUDecodeStage())
  private val fpuExecuteStage = Module(new FPUMulStage())

  io.in <> fpuDecodeStage.io.in
  fpuDecodeStage.io.out <> fpuExecuteStage.io.in
  fpuExecuteStage.io.out <> io.out

  fpuDecodeStage.io.recover := io.recover
  fpuExecuteStage.io.recover := io.recover
}

class FPUDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new FPUMulStageEntry())

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

  // op
  io.out.bits.op := FPUOperationType.undefined
  switch(inReg.opcode(6, 2)) {
    is("b10100".U) { // Basic instructions
      switch(inReg.func7(6, 2)) {
        is("b00000".U) { // fadd
          io.out.bits.op := FPUOperationType.add
        }
        is("b00001".U) { // fsub
          io.out.bits.op := FPUOperationType.sub
        }
        is("b11100".U) { // fmv.x.w
          io.out.bits.op := FPUOperationType.mvXW
        }
        is("b11110".U) { //  fmv.w.x
          io.out.bits.op := FPUOperationType.mvWX
        }
      }
    }
  }

  // FP unbox

  private def fp32Unbox(in: UInt, out: FPOperand) = {
    val fpUnbox = Module(new FPUnbox(IEEESpec32))
    fpUnbox.io.in := in

    val fpValid = extractBits(CoreConstant.wordWidth)(in, 1).andR // NaN unbox
    out.raw := in
    out.entry := Mux(fpValid, fpUnbox.io.out, FP.nan)
    out.fpClass := Mux(fpValid, fpUnbox.io.fpClass, FPClassCode.quietNaN.U);
  }

  private def fp64Unbox(in: UInt, out: FPOperand) = {
    val fpUnbox = Module(new FPUnbox(IEEESpec64))
    fpUnbox.io.in := in
    out.raw := in
    out.entry := fpUnbox.io.out
    out.fpClass := fpUnbox.io.fpClass
  }

  io.out.bits.fpOp1 := new FPOperand().zero
  io.out.bits.fpOp2 := new FPOperand().zero
  io.out.bits.fpOp3 := new FPOperand().zero
  switch(inReg.func7(1, 0)) {
    is("b00".U) { // Single-precision floating-point number
      fp32Unbox(inReg.rs1.receipt, io.out.bits.fpOp1)
      fp32Unbox(inReg.rs2.receipt, io.out.bits.fpOp2)
      fp32Unbox(inReg.rs3.receipt, io.out.bits.fpOp3)
    }
    is("b01".U) { // Double-precision floating-point number
      fp64Unbox(inReg.rs1.receipt, io.out.bits.fpOp1)
      fp64Unbox(inReg.rs2.receipt, io.out.bits.fpOp2)
      fp64Unbox(inReg.rs3.receipt, io.out.bits.fpOp3)
    }
  }

  // xOp
  io.out.bits.xOp := inReg.rs1.receipt

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

class FPUMulStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new FPUMulStageEntry()))
    val out = DecoupledIO(new FPUAddStageEntry())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = RegInit(new FPUMulStageEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  private val fpMul = Module(new FPMul())
  fpMul.io.in1 := inReg.fpOp1.entry
  fpMul.io.in2 := inReg.fpOp2.entry

  switch(inReg.op) {
    is(FPUOperationType.madd) {
      io.out.bits.fpOp1 := fpMul.io.out
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.msub) {
      io.out.bits.fpOp1 := fpMul.io.out
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.nmadd) {
      FP.neg(fpMul.io.out, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.nmsub) {
      FP.neg(fpMul.io.out, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.add) {
      FP.extend(inReg.fpOp1.entry, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp2.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.sub) {
      FP.extend(inReg.fpOp1.entry, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp2.entry, io.out.bits.fpOp2)
    }
    is(FPUOperationType.mul) {
      io.out.bits.fpOp1 := fpMul.io.out
    }
    is(FPUOperationType.div) {}
    is(FPUOperationType.sqrt) {}
    is(FPUOperationType.mvWX) {
      io.out.bits.xOp := inReg.xOp
    }
    is(FPUOperationType.mvXW) {
      FP.extend(inReg.fpOp1.entry, io.out.bits.fpOp1)
    }
  }

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

class FPUAddStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new FPUAddStageEntry()))
    val out = DecoupledIO(new FPUBoxStageEntry())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = RegInit(new FPUAddStageEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

class FPUBoxStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new FPUBoxStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new FPUBoxStageEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.out.bits := new ExecuteResultEntry().zero
  switch(inReg.op) {
    is(FPUOperationType.mvXW) {
      io.out.bits.result := CoreUtils.extractBits(CoreConstant.wordWidth)(inReg.raw, 0)
    }

    is(FPUOperationType.mvWX) {
      // NaN box
      io.out.bits.result := Fill(CoreConstant.wordWidth, 1.U) ## CoreUtils.extractBits(CoreConstant.wordWidth)(inReg.xOp, 0)
    }
  }

  // Exception handler
  when(inReg.op === FPUOperationType.undefined) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
  }

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
