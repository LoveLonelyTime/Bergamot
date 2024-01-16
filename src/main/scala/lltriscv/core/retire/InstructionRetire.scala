package lltriscv.core.retire

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.record.ROBTableRetireIO
import lltriscv.core.record.RegisterUpdateIO
import lltriscv.core.record.CSRsWriteIO
import lltriscv.core.record.ExceptionRequestIO

/*
 * Instruction retire
 *
 * When an instruction is in a non speculative and committed status, it will be retired,
 * and the retired instruction will actually change the core status.
 * It will detect the results of retired instructions, such as whether they cause prediction failures,
 * whether they trigger exceptions, and write memory.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Instruction retire
  *
  * @param depth
  *   ROB table depth
  */
class InstructionRetire(depth: Int) extends Module {
  require(depth > 0, "ROB table depth must be greater than 0")
  val io = IO(new Bundle {
    // Retired interface
    val retired = Flipped(DecoupledIO(DataType.receipt))
    // Table retire interface
    val tableRetire = Flipped(new ROBTableRetireIO(depth))
    // Register update interface
    val update = new RegisterUpdateIO()
    // Recovery interface
    val recover = Output(new Bool())
    val correctPC = Output(DataType.address)
    // CSR write interface
    val csr = new CSRsWriteIO()
    // Exception interface
    val exception = new ExceptionRequestIO()
  })

  private val retireEntries =
    List(
      io.tableRetire.entries(io.retired.bits(30, 0) ## 0.U),
      io.tableRetire.entries(io.retired.bits(30, 0) ## 1.U)
    )

  private val retireValid = List(
    retireEntries(0).commit ||
      !retireEntries(0).valid,
    retireEntries(1).commit ||
      !retireEntries(1).valid
  )

  io.recover := false.B
  io.correctPC := 0.U
  io.update.entries.foreach(item => {
    item.rd := 0.U
    item.result := 0.U
  })
  io.csr.wen := false.B
  io.csr.address := 0.U
  io.csr.data := 0.U

  io.exception.xret := false.B
  io.exception.trigger := false.B
  io.exception.exceptionPC := 0.U
  io.exception.exceptionVal := 0.U
  io.exception.exceptionCode := 0.U

  def gotoExceptionHandler(id: Int) = {
    io.recover := true.B

    io.exception.trigger := true.B
    io.exception.exceptionPC := retireEntries(id).pc
    io.exception.exceptionVal := 0.U
    io.exception.exceptionCode := retireEntries(id).executeResult.exceptionCode
    io.correctPC := io.exception.handlerPC
    printf("Exception!!!! pc = %d\n", retireEntries(id).pc)
  }

  def gotoRecoveryPath(id: Int) = {
    io.recover := true.B
    io.correctPC := retireEntries(id).executeResult.real
    printf(
      "spec violate!!!: pc = %d, sepc = %d, real = %d\n",
      retireEntries(id).pc,
      retireEntries(id).spec,
      retireEntries(id).executeResult.real
    )
  }

  def gotoXRetPath(id: Int) = {
    io.exception.xret := true.B
    io.recover := true.B
    io.correctPC := io.exception.handlerPC

    printf(
      "xret !!!: pc = %d\n",
      retireEntries(id).pc
    )
  }

  def gotoCSRPath(id: Int) = {
    io.recover := true.B
    io.correctPC := retireEntries(id).executeResult.real
  }

  def hasException(id: Int) = retireEntries(id).valid && retireEntries(id).executeResult.exception
  def hasBranch(id: Int) = retireEntries(id).valid && retireEntries(id).executeResult.real =/= retireEntries(id).spec

  def hasCSR(id: Int) = retireEntries(id).valid && retireEntries(id).executeResult.writeCSR
  def hasXRet(id: Int) = retireEntries(id).valid && retireEntries(id).executeResult.xret

  def updateRegister(id: Int) = {
    io.update.entries(id).rd := retireEntries(id).rd
    io.update.entries(id).result := retireEntries(id).executeResult.result

    printf(
      "retired instruction: pc = %d , r = %d, v = %d\n",
      retireEntries(id).pc,
      retireEntries(id).executeResult.result,
      retireEntries(id).valid
    )
  }

  def writeCSRs(id: Int) = {
    io.csr.wen := true.B
    io.csr.address := retireEntries(id).executeResult.csrAddress
    io.csr.data := retireEntries(id).executeResult.csrData
  }

  io.retired.ready := false.B
  when(io.retired.valid && retireValid(0) && retireValid(1)) {
    io.retired.ready := true.B

    when(hasException(0)) { // Exception ?
      gotoExceptionHandler(0)
    }.elsewhen(hasXRet(0)) { // XRet ?
      gotoXRetPath(0)
    }.elsewhen(hasCSR(0)) { // CSR ?
      writeCSRs(0)
      updateRegister(0)
      gotoCSRPath(0)
    }.elsewhen(hasBranch(0)) { // Branch ?
      updateRegister(0)
      gotoRecoveryPath(0)
    }.otherwise { // Normal 0
      when(retireEntries(0).valid) {
        updateRegister(0)
      }

      when(hasException(1)) { // Exception ?
        gotoExceptionHandler(1)
      }.elsewhen(hasXRet(1)) { // XRet ?
        gotoXRetPath(1)
      }.elsewhen(hasCSR(1)) { // CSR ?
        writeCSRs(1)
        updateRegister(1)
        gotoCSRPath(1)
      }.elsewhen(hasBranch(1)) { // Branch ?
        updateRegister(1)
        gotoRecoveryPath(1)
      }.elsewhen(retireEntries(1).valid) { // Normal 1
        updateRegister(1)
      }
    }
  }
}
