package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.utils.ChiselUtils._

/*
 * CSRs (Control and Status Registers) unit, is used to save the core and machine state
 *
 * A CSR is implemented by a register or a functional class
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** CSRs component
  *
  * Provide addressable read and write ports and fixed CSRs output.
  *
  * Responsible for switching states in exceptions and interrupts.
  */
class CSRs extends Module {
  val io = IO(new Bundle {
    // Write interface
    val write = Flipped(new CSRsWriteIO())
    // Read interface
    val read = new CSRsReadIO()
    // Exception
    val exception = Flipped(new ExceptionRequestIO())
    // Fixed CSRs output
    val privilege = Output(PrivilegeType()) // Current core privilege
    val mstatus = Output(DataType.operation)
    val satp = Output(DataType.operation)
  })

  // M-Mode CSRs
  private val mstatusReg = new MStatusRegister()
  private val mtvecReg = RegInit(DataType.operation.zeroAsUInt)
  private val medelegReg = RegInit(DataType.operation.zeroAsUInt)
  private val mepcReg = RegInit(DataType.operation.zeroAsUInt)
  private val mcauseReg = RegInit(DataType.operation.zeroAsUInt)
  private val mtvalReg = RegInit(DataType.operation.zeroAsUInt)

  // S-Mode CSRs
  private val satpReg = RegInit(DataType.operation.zeroAsUInt)
  private val stvecReg = RegInit(DataType.operation.zeroAsUInt)
  private val sepcReg = RegInit(DataType.operation.zeroAsUInt)
  private val scauseReg = RegInit(DataType.operation.zeroAsUInt)
  private val stvalReg = RegInit(DataType.operation.zeroAsUInt)

  // Fixed output
  io.satp := satpReg
  io.privilege := mstatusReg.privilege
  io.mstatus := mstatusReg.mstatusView

  // Read logic
  def responseRead(content: UInt) = {
    io.read.error := false.B
    io.read.data := content
  }
  io.read.error := true.B
  io.read.data := 0.U
  switch(io.read.address) {
    is("h105".U) { responseRead(stvecReg) } // stvec

    is("h141".U) { responseRead(sepcReg) } // sepc
    is("h142".U) { responseRead(scauseReg) } // scause
    is("h143".U) { responseRead(stvalReg) } // stval

    is("h180".U) { responseRead(satpReg) } // satp

    is("h300".U) { responseRead(mstatusReg.mstatusView) } // mstatus
    is("h302".U) { responseRead(medelegReg) } // medeleg
    is("h305".U) { responseRead(mtvecReg) } // mtvec

    is("h341".U) { responseRead(mepcReg) } // mepc
    is("h342".U) { responseRead(mcauseReg) } // mcause
    is("h343".U) { responseRead(mtvalReg) } // mtval
  }

  // Write logic
  when(io.write.wen) {
    switch(io.write.address) {
      is("h105".U) { stvecReg := io.write.data } // stvec

      is("h141".U) { sepcReg := io.write.data } // sepc
      is("h142".U) { scauseReg := io.write.data } // scause
      is("h143".U) { stvalReg := io.write.data } // stval

      is("h180".U) { satpReg := io.write.data } // satp

      is("h300".U) { mstatusReg.writeMStatus(io.write.data) } // mstatus
      is("h302".U) { medelegReg := io.write.data } // medeleg
      is("h305".U) { mtvecReg := io.write.data } // mtvec

      is("h341".U) { mepcReg := io.write.data } // mepc
      is("h342".U) { mcauseReg := io.write.data } // mcause
      is("h343".U) { mtvalReg := io.write.data } // mtval
    }
  }

  // Exception and privilege logic
  io.exception.handlerPC := 0.U
  when(io.exception.trigger) {
    // Delegation check
    val delegation = medelegReg(io.exception.exceptionCode)
    when(delegation) { // S-Handler
      sepcReg := io.exception.exceptionPC
      scauseReg := io.exception.exceptionCode
      stvalReg := io.exception.exceptionVal
      io.exception.handlerPC := stvecReg(31, 2) ## 0.U(2.W)
      mstatusReg.trapToSLevel()
    }.otherwise { // M-Handler
      mepcReg := io.exception.exceptionPC
      mcauseReg := io.exception.exceptionCode
      mtvalReg := io.exception.exceptionVal
      io.exception.handlerPC := mtvecReg(31, 2) ## 0.U(2.W)
      mstatusReg.trapToMLevel()
    }
  }

  // xret
  when(io.exception.xret) {
    switch(mstatusReg.privilege) {
      is(PrivilegeType.M) { // mret
        io.exception.handlerPC := mepcReg
        mstatusReg.returnFromM()
      }
      is(PrivilegeType.S) { // sret
        io.exception.handlerPC := sepcReg
        mstatusReg.returnFromS()
      }
    }
  }
}

/** MStatus registers (mstatus, mstatush, privilege)
  */
class MStatusRegister {
  private val mstatusReg = RegInit(DataType.operation.zeroAsUInt)
  private val mstatushReg = RegInit(DataType.operation.zeroAsUInt)
  private val privilegeReg = RegInit(PrivilegeType.M)

  def privilege = privilegeReg

  def trapToMLevel() = {
    privilegeReg := PrivilegeType.M

    val mpp = WireInit(0.U(2.W))
    switch(privilegeReg) {
      is(PrivilegeType.M) { mpp := "b11".U }
      is(PrivilegeType.S) { mpp := "b01".U }
      is(PrivilegeType.U) { mpp := "b00".U }
    }
    val mpie = mstatusReg(3)
    val mie = 0.U
    mstatusReg := mstatusReg(31, 13) ##
      mpp ##
      mstatusReg(10, 8) ##
      mpie ##
      mstatusReg(6, 4) ##
      mie ##
      mstatusReg(2, 0)
  }

  def trapToSLevel() = {
    privilegeReg := PrivilegeType.S

    val spp = WireInit(0.U)
    switch(privilegeReg) {
      is(PrivilegeType.S) { spp := 1.U }
      is(PrivilegeType.U) { spp := 0.U }
    }
    val spie = mstatusReg(1)
    val sie = 0.U

    mstatusReg := mstatusReg(31, 9) ##
      spp ##
      mstatusReg(7, 6) ##
      spie ##
      mstatusReg(4, 2) ##
      sie ##
      mstatusReg(0)
  }

  def returnFromM() = {
    val mprv = WireInit(mstatusReg(17))
    switch(mstatusReg(12, 11)) { // mpp
      is("b11".U) { privilegeReg := PrivilegeType.M }
      is("b01".U) {
        privilegeReg := PrivilegeType.S
        mprv := 0.U
      }
      is("b00".U) {
        privilegeReg := PrivilegeType.U
        mprv := 0.U
      }
    }

    mstatusReg := mstatusReg(31, 18) ##
      mprv ## // mprv
      mstatusReg(16, 13) ##
      0.U(2.W) ## // mpp
      mstatusReg(10, 8) ##
      1.U ## // mpie
      mstatusReg(6, 4) ##
      mstatusReg(7) ## // mie
      mstatusReg(2, 0)
  }

  def returnFromS() = {
    when(mstatusReg(8)) { // spp
      privilegeReg := PrivilegeType.S
    }.otherwise {
      privilegeReg := PrivilegeType.U
    }

    mstatusReg := mstatusReg(31, 18) ##
      0.U ## // mprv
      mstatusReg(16, 9) ##
      0.U ## // spp
      mstatusReg(7, 6) ##
      1.U ## // spie
      mstatusReg(4, 2) ##
      mstatusReg(5) ## // sie
      mstatusReg(0)
  }
  def mstatusView = mstatusReg

  def writeMStatus(content: UInt) = mstatusReg := content
}
