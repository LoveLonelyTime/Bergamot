package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

class CSRs extends Module {
  val io = IO(new Bundle {
    // Write interface
    val write = Flipped(new CSRsWriteIO())
    // Read interface
    val read = new CSRsReadIO()
    // Exception
    val exception = Flipped(new ExceptionRequestIO())
    // Current core privilege
    val privilege = Output(PrivilegeType())
    // Read interface
    val satp = Output(DataType.operation)

  })

  // M-Mode CSRs
  private val mstatusReg = new MStatusRegister()
  private val mtvec = Reg(DataType.operation)
  private val medeleg = Reg(DataType.operation)
  private val mepc = Reg(DataType.operation)
  private val mcause = Reg(DataType.operation)
  private val mtval = Reg(DataType.operation)

  // S-Mode CSRs
  private val satpReg = Reg(DataType.operation)
  private val stvec = Reg(DataType.operation)
  private val sepc = Reg(DataType.operation)
  private val scause = Reg(DataType.operation)
  private val stval = Reg(DataType.operation)

  io.satp := satpReg

  io.privilege := mstatusReg.privilege

  // Read logic
  def responseRead(content: UInt) = {
    io.read.error := false.B
    io.read.data := content
  }
  io.read.error := true.B
  io.read.data := 0.U
  switch(io.read.address) {
    is("h105".U) { responseRead(stvec) } // stvec

    is("h141".U) { responseRead(sepc) } // sepc
    is("h142".U) { responseRead(scause) } // scause
    is("h143".U) { responseRead(stval) } // stval

    is("h300".U) { responseRead(mstatusReg.mstatusView) } // mstatus
    is("h302".U) { responseRead(medeleg) } // medeleg
    is("h305".U) { responseRead(mtvec) } // mtvec

    is("h341".U) { responseRead(mepc) } // mepc
    is("h342".U) { responseRead(mcause) } // mcause
    is("h343".U) { responseRead(mtval) } // mtval
  }

  // Write logic
  when(io.write.wen) {
    switch(io.write.address) {
      is("h105".U) { stvec := io.write.data } // stvec

      is("h141".U) { sepc := io.write.data } // sepc
      is("h142".U) { scause := io.write.data } // scause
      is("h143".U) { stval := io.write.data } // stval

      is("h300".U) { mstatusReg.writeMStatus(io.write.data) } // mstatus
      is("h302".U) { medeleg := io.write.data } // medeleg
      is("h305".U) { mtvec := io.write.data } // mtvec

      is("h341".U) { mepc := io.write.data } // mepc
      is("h342".U) { mcause := io.write.data } // mcause
      is("h343".U) { mtval := io.write.data } // mtval
    }
  }

  // Exception and privilege logic
  io.exception.handlerPC := 0.U
  when(io.exception.trigger) {
    // Delegation check
    val delegation = medeleg(io.exception.exceptionCode)
    when(delegation) { // S-Handler
      sepc := io.exception.exceptionPC
      scause := io.exception.exceptionCode
      stval := io.exception.exceptionVal
      io.exception.handlerPC := stvec(31, 2) ## 0.U(2.W)
      mstatusReg.trapToSLevel()
    }.otherwise { // M-Handler
      mepc := io.exception.exceptionPC
      mcause := io.exception.exceptionCode
      mtval := io.exception.exceptionVal
      io.exception.handlerPC := mtvec(31, 2) ## 0.U(2.W)
      mstatusReg.trapToMLevel()
    }
  }

  // xret
  when(io.exception.xret) {
    switch(mstatusReg.privilege) {
      is(PrivilegeType.M) { // mret
        io.exception.handlerPC := mepc
        mstatusReg.returnFromM()
      }
      is(PrivilegeType.S) { // sret
        io.exception.handlerPC := sepc
        mstatusReg.returnFromS()
      }
    }
  }
}

class MStatusRegister {
  private val mstatusReg = Reg(DataType.operation)
  private val mstatushReg = Reg(DataType.operation)
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
