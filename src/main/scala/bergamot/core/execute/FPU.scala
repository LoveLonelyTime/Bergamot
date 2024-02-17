package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.record.ExceptionCode
import bergamot.core.fpu.IEEESpec64
import bergamot.core.fpu.IEEESpec32
import bergamot.core.fpu.FP
import bergamot.core.fpu.FPClassCode
import bergamot.core.fpu.FPMul
import bergamot.core.fpu.FPAdd
import bergamot.core.fpu.FPDiv
import bergamot.core.fpu.FPSqrt
import bergamot.core.fpu.FPBox
import bergamot.core.fpu.FPUnbox

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._
import bergamot.core.fpu.FPRoundoff
import bergamot.core.fpu.FPMulEntry
import bergamot.core.fpu.FPAddEntry
import bergamot.core.fpu.FPException

class FPU extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Float dynamic rounding mode
    val fcsr = Input(DataType.operation)
    // Recovery interface
    val recover = Input(Bool())
  })

  private val fpuDecodeStage = Module(new FPUDecodeStage())
  private val fpuMulStage = Module(new FPUMulStage())
  private val fpuAddStage = Module(new FPUAddStage())
  private val fpuBoxStage = Module(new FPUBoxStage())

  io.in <> fpuDecodeStage.io.in
  fpuDecodeStage.io.out <> fpuMulStage.io.in
  fpuMulStage.io.out <> fpuAddStage.io.in
  fpuAddStage.io.out <> fpuBoxStage.io.in
  fpuBoxStage.io.out <> io.out

  fpuBoxStage.io.fcsr := io.fcsr

  fpuDecodeStage.io.recover := io.recover
  fpuMulStage.io.recover := io.recover
  fpuAddStage.io.recover := io.recover
  fpuBoxStage.io.recover := io.recover
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
    is("b10000".U) {
      io.out.bits.op := FPUOperationType.madd
    }
    is("b10001".U) {
      io.out.bits.op := FPUOperationType.msub
    }
    is("b10010".U) {
      io.out.bits.op := FPUOperationType.nmsub
    }
    is("b10011".U) {
      io.out.bits.op := FPUOperationType.nmadd
    }
    is("b10100".U) { // Basic instructions
      io.out.bits.op := MuxCase(
        FPUOperationType.undefined,
        Seq(
          (inReg.func7(6, 2) === "b00000".U) -> FPUOperationType.add,
          (inReg.func7(6, 2) === "b00001".U) -> FPUOperationType.sub,
          (inReg.func7(6, 2) === "b00010".U) -> FPUOperationType.mul,
          (inReg.func7(6, 2) === "b00011".U) -> FPUOperationType.div,
          (inReg.func7(6, 2) === "b01011".U) -> FPUOperationType.sqrt,
          (inReg.func7(6, 2) === "b11100".U && inReg.func3 === 0.U) -> FPUOperationType.mvXW,
          (inReg.func7(6, 2) === "b11110".U) -> FPUOperationType.mvWX
        )
      )
    }
  }

  // FP width
  io.out.bits.fpWidth := MuxLookup(inReg.func7(1, 0), FPUOperationWidth.undefined)(
    Seq(
      "b00".U -> FPUOperationWidth.float, // Single-precision floating-point number
      "b01".U -> FPUOperationWidth.double // Double-precision floating-point number
    )
  )

  // FP RM
  io.out.bits.fpRm := inReg.func3

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
  switch(io.out.bits.fpWidth) {
    is(FPUOperationWidth.float) {
      fp32Unbox(inReg.rs1.receipt, io.out.bits.fpOp1)
      fp32Unbox(inReg.rs2.receipt, io.out.bits.fpOp2)
      fp32Unbox(inReg.rs3.receipt, io.out.bits.fpOp3)
    }
    is(FPUOperationWidth.double) {
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

  private val fpException = WireInit(new FPException().zero)

  private val fpMul = Module(new FPMul())
  fpMul.io.in1 := inReg.fpOp1.entry
  fpMul.io.in2 := inReg.fpOp2.entry

  private val fpDiv = Module(new FPDiv())
  fpDiv.io.in1 := inReg.fpOp1.entry
  fpDiv.io.in2 := inReg.fpOp2.entry

  private val fpSqrt = Module(new FPSqrt())
  fpSqrt.io.in := inReg.fpOp1.entry

  io.out.bits.fpOp1 := new FPMulEntry().zero
  io.out.bits.fpOp2 := new FPMulEntry().zero
  io.out.bits.fpRes := new FPAddEntry().zero
  io.out.bits.xOp := 0.U
  switch(inReg.op) {
    is(FPUOperationType.madd) {
      io.out.bits.fpOp1 := fpMul.io.out
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
      fpException := fpMul.io.exception
    }
    is(FPUOperationType.msub) {
      io.out.bits.fpOp1 := fpMul.io.out
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
      fpException := fpMul.io.exception
    }
    is(FPUOperationType.nmadd) {
      FP.neg(fpMul.io.out, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
      fpException := fpMul.io.exception
    }
    is(FPUOperationType.nmsub) {
      FP.neg(fpMul.io.out, io.out.bits.fpOp1)
      FP.extend(inReg.fpOp3.entry, io.out.bits.fpOp2)
      fpException := fpMul.io.exception
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
      fpException := fpMul.io.exception
    }
    is(FPUOperationType.div) {
      io.out.bits.fpRes := fpDiv.io.out
      fpException := fpDiv.io.exception
    }
    is(FPUOperationType.sqrt) {
      io.out.bits.fpRes := fpSqrt.io.out
      fpException := fpSqrt.io.exception
    }
    is(FPUOperationType.mvWX) {
      io.out.bits.xOp := inReg.xOp
    }
    is(FPUOperationType.mvXW) {
      io.out.bits.xOp := inReg.fpOp1.raw
    }
  }

  io.out.bits.op := inReg.op
  io.out.bits.fpWidth := inReg.fpWidth
  io.out.bits.fpRm := inReg.fpRm
  io.out.bits.exception := fpException

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

  private val fpAdd = Module(new FPAdd())
  fpAdd.io.in1 := new FPMulEntry().zero
  fpAdd.io.in2 := new FPMulEntry().zero

  private val fpException = WireInit(new FPException().zero)

  io.out.bits.fpOp := new FPAddEntry().zero
  io.out.bits.xOp := 0.U
  switch(inReg.op) {
    is(FPUOperationType.madd) {
      fpAdd.io.in1 := inReg.fpOp1
      fpAdd.io.in2 := inReg.fpOp2
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.msub) {
      fpAdd.io.in1 := inReg.fpOp1
      FP.neg(inReg.fpOp2, fpAdd.io.in2)
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.nmadd) {
      fpAdd.io.in1 := inReg.fpOp1
      FP.neg(inReg.fpOp2, fpAdd.io.in2)
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.nmsub) {
      fpAdd.io.in1 := inReg.fpOp1
      fpAdd.io.in2 := inReg.fpOp2
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.add) {
      fpAdd.io.in1 := inReg.fpOp1
      fpAdd.io.in2 := inReg.fpOp2
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.sub) {
      fpAdd.io.in1 := inReg.fpOp1
      FP.neg(inReg.fpOp2, fpAdd.io.in2)
      io.out.bits.fpOp := fpAdd.io.out
      fpException := fpAdd.io.exception
    }
    is(FPUOperationType.mul) {
      FP.extend(inReg.fpOp1, io.out.bits.fpOp)
    }
    is(FPUOperationType.div) {
      io.out.bits.fpOp := inReg.fpRes
    }
    is(FPUOperationType.sqrt) {
      io.out.bits.fpOp := inReg.fpRes
    }
    is(FPUOperationType.mvWX) {
      io.out.bits.xOp := inReg.xOp
    }
    is(FPUOperationType.mvXW) {
      io.out.bits.xOp := inReg.xOp
    }
  }

  io.out.bits.op := inReg.op
  io.out.bits.fpWidth := inReg.fpWidth
  io.out.bits.fpRm := inReg.fpRm

  io.out.bits.exception.invalidOperation := inReg.exception.invalidOperation || fpException.invalidOperation
  io.out.bits.exception.divideByZero := inReg.exception.divideByZero || fpException.divideByZero
  io.out.bits.exception.overflow := inReg.exception.overflow || fpException.overflow
  io.out.bits.exception.underflow := inReg.exception.underflow || fpException.underflow
  io.out.bits.exception.inexact := inReg.exception.inexact || fpException.inexact

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

class FPUBoxStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new FPUBoxStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Float dynamic rounding mode
    val fcsr = Input(DataType.operation)
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

  io.in.ready := io.out.ready

  io.out.bits := new ExecuteResultEntry().zero

  private def getRoundingMode(rm: UInt) = {
    val result = Wire(FPRoundoff())
    result := MuxLookup(rm, FPRoundoff.RNE)(
      Seq(
        "b000".U -> FPRoundoff.RNE,
        "b001".U -> FPRoundoff.RTZ,
        "b010".U -> FPRoundoff.RDN,
        "b011".U -> FPRoundoff.RUP,
        "b100".U -> FPRoundoff.RMM
      )
    )

    // Reserved
    when(rm in ("b101".U, "b110".U, "b111".U)) {
      io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
    }

    result
  }

  private val roundingMode = Mux(inReg.fpRm === "b111".U, getRoundingMode(io.fcsr(7, 5)), getRoundingMode(inReg.fpRm))

  private val fpBox32 = Module(new FPBox(IEEESpec32))
  fpBox32.io.roundoff := roundingMode
  fpBox32.io.in := inReg.fpOp

  private val fpBox64 = Module(new FPBox(IEEESpec32))
  fpBox64.io.roundoff := roundingMode
  fpBox64.io.in := inReg.fpOp

  private val fpBoxException = WireInit(new FPException().zero)

  // FFlags
  private val fpException = Wire(new FPException())
  fpException.invalidOperation := inReg.exception.invalidOperation || fpBoxException.invalidOperation || io.fcsr(4)
  fpException.divideByZero := inReg.exception.divideByZero || fpBoxException.divideByZero || io.fcsr(3)
  fpException.overflow := inReg.exception.overflow || fpBoxException.overflow || io.fcsr(2)
  fpException.underflow := inReg.exception.underflow || fpBoxException.underflow || io.fcsr(1)
  fpException.inexact := inReg.exception.inexact || fpBoxException.inexact || io.fcsr(0)

  private val fflags = fpException.invalidOperation ## fpException.divideByZero ## fpException.overflow ## fpException.underflow ## fpException.inexact

  private val signalingException = WireInit(false.B)
  when(fflags =/= io.fcsr(4, 0) && signalingException) {
    io.out.bits.resultCSR("h001".U, fflags) // fflags
  }

  private def box() = {
    switch(inReg.fpWidth) {
      is(FPUOperationWidth.float) {
        // NaN box
        io.out.bits.result := Fill(CoreConstant.wordWidth, 1.U) ## fpBox32.io.out
        fpBoxException := fpBox32.io.exception
      }
      is(FPUOperationWidth.double) {
        io.out.bits.result := fpBox64.io.out
        fpBoxException := fpBox64.io.exception
      }
      is(FPUOperationWidth.undefined) {
        io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
      }
    }
  }

  switch(inReg.op) {
    is(FPUOperationType.madd) { box(); signalingException := true.B }
    is(FPUOperationType.msub) { box(); signalingException := true.B }
    is(FPUOperationType.nmadd) { box(); signalingException := true.B }
    is(FPUOperationType.nmsub) { box(); signalingException := true.B }
    is(FPUOperationType.add) { box(); signalingException := true.B }
    is(FPUOperationType.sub) { box(); signalingException := true.B }
    is(FPUOperationType.mul) { box(); signalingException := true.B }
    is(FPUOperationType.div) { box(); signalingException := true.B }
    is(FPUOperationType.sqrt) { box(); signalingException := true.B }

    is(FPUOperationType.mvXW) {
      io.out.bits.result := extractBits(CoreConstant.wordWidth)(inReg.xOp, 0)
    }

    is(FPUOperationType.mvWX) {
      // NaN box
      io.out.bits.result := Fill(CoreConstant.wordWidth, 1.U) ## extractBits(CoreConstant.wordWidth)(inReg.xOp, 0)
    }
  }

  // Exception handler
  when(inReg.op === FPUOperationType.undefined) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
  }

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
