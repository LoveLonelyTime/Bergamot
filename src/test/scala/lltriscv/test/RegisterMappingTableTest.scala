package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import chisel3.util._

import lltriscv.core._
import lltriscv.core.decode._
import lltriscv.core.record._
import lltriscv.core.execute._
import lltriscv.core.broadcast.DataBroadcastIO

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "RegisterMappingTable should be OK" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
        val broadcast = Flipped(new DataBroadcastIO())
        val retired = DecoupledIO(DataType.receiptType)

        val memoryDeq = DecoupledIO(new ExecuteEntry())
        val aluDeq = DecoupledIO(new ExecuteEntry())
        val branchDeq = DecoupledIO(new ExecuteEntry())
      })
      private val registerMappingTable = Module(new RegisterMappingTable())
      private val rob = Module(new ROB(8))
      private val instructionDecoder = Module(new InstructionDecoder())
      private val aluExecuteQueue = Module(new InOrderExecuteQueue(8))
      private val memoryExecuteQueue = Module(new InOrderExecuteQueue(8))
      private val branchExecuteQueue = Module(new InOrderExecuteQueue(8))

      aluExecuteQueue.io.deq <> io.aluDeq
      memoryExecuteQueue.io.deq <> io.memoryDeq
      branchExecuteQueue.io.deq <> io.branchDeq

      aluExecuteQueue.io.broadcast <> io.broadcast
      memoryExecuteQueue.io.broadcast <> io.broadcast
      branchExecuteQueue.io.broadcast <> io.broadcast

      registerMappingTable.io.mapping <> instructionDecoder.io.mapping
      registerMappingTable.io.alloc <> rob.io.alloc
      registerMappingTable.io.broadcast <> io.broadcast
      rob.io.retired <> io.retired

      instructionDecoder.io.broadcast <> io.broadcast

      instructionDecoder.io.in <> io.in
      instructionDecoder.io.memoryEnq <> memoryExecuteQueue.io.enq
      instructionDecoder.io.aluEnq <> aluExecuteQueue.io.enq
      instructionDecoder.io.branchEnq <> branchExecuteQueue.io.enq
    }).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rs2 rs1 rd
      dut.io.in
        .bits(0)
        .instruction
        .poke("b0000000_00001_00001_000_00001_0110011".U)
      dut.io.in
        .bits(0)
        .vaild
        .poke(true.B)

      dut.io.in
        .bits(1)
        .instruction
        .poke("b0000000_00001_00001_000_00001_0110011".U)
      dut.io.in
        .bits(1)
        .vaild
        .poke(true.B)

      dut.io.in.valid.poke(true.B)

      dut.clock.step(50)
    }
  }
}
