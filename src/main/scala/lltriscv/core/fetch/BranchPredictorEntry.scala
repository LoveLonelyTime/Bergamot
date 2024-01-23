package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.utils.ChiselUtils._

class BranchPredictorRequestIO extends Bundle {
  val in = Output(
    Vec(
      2,
      new Bundle {
        val pc = DataType.address
        val compress = Bool()
      }
    )
  )

  val out = Input(Vec(2, DataType.address))
}

class BranchPredictorUpdateIO extends Bundle {
  val entries = Output(
    Vec(
      2,
      new Bundle {
        val pc = DataType.address
        val address = DataType.address
        val jump = Bool()
        val valid = Bool()
      }
    )
  )
}
