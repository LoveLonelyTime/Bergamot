package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import chisel3.util._

import lltriscv.core._
import lltriscv.core.decode._
import lltriscv.core.record._

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "RegisterMappingTable should be OK" in {
    test(new Module {
      val io = IO(new Bundle {
        val mapping = Flipped(new RegisterMappingIO())
        val broadcast = Flipped(new DataBroadcastIO())
        val retired = DecoupledIO(DataType.receiptType)
      })
      private val registerMappingTable = Module(new RegisterMappingTable())
      private val rob = Module(new ROB(8))
      registerMappingTable.io.mapping <> io.mapping
      registerMappingTable.io.alloc <> rob.io.alloc
      registerMappingTable.io.broadcast <> io.broadcast
      rob.io.retired <> io.retired
    }).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.mapping.valid.poke(true.B)
      dut.io.mapping.regGroup(0).rs1.poke(3.U)
      dut.io.mapping.regGroup(0).rs2.poke(4.U)
      dut.io.mapping.regGroup(0).rd.poke(3.U)

      dut.io.mapping.regGroup(1).rs1.poke(3.U)
      dut.io.mapping.regGroup(1).rs2.poke(3.U)
      dut.io.mapping.regGroup(1).rd.poke(4.U)

      dut.clock.step(10)
    }
  }
}
