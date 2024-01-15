package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

class CSRs extends Module {
  val io = IO(new Bundle {
    // Write interface
    val write = new CSRsWriteIO()
    // Current core privilege
    val privilege = Output(PrivilegeType())
    // Read interface
    val satp = Output(DataType.operation)

  })

  // S-Mode CSRs
  private val satpReg = Reg(DataType.operation)
  io.satp := satpReg
}
