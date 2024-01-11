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
import lltriscv.core.broadcast.PriorityBroadcaster
import lltriscv.core.decode.InstructionDecoder

class RetireMock extends Module {
  val io = IO(new Bundle {
    val retired = Flipped(DecoupledIO(DataType.receiptType))
    val tableRetire = Flipped(new ROBTableRetireIO(8))
  })

  private val id1 = io.retired.bits(30, 0) ## 0.U
  private val id2 = io.retired.bits(30, 0) ## 1.U

  private val retire1 =
    io.tableRetire.entries(id1).commit || !io.tableRetire.entries(id1).valid

  private val retire2 =
    io.tableRetire.entries(id2).commit || !io.tableRetire.entries(id2).valid

  io.retired.ready := false.B
  when(io.retired.valid && retire1 && retire2) {
    io.retired.ready := true.B
    printf(
      "retired instruction: \n pc = %d , r = %d, v = %d \n pc = %d , r = %d, v = %d \n",
      io.tableRetire.entries(id1).pc,
      io.tableRetire.entries(id1).result,
      io.tableRetire.entries(id1).valid,
      io.tableRetire.entries(id2).pc,
      io.tableRetire.entries(id2).result,
      io.tableRetire.entries(id2).valid
    )
  }
}

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "RegisterMappingTable should be OK" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
      })
      private val registerMappingTable = Module(new RegisterMappingTable())
      private val rob = Module(new ROB(8))
      private val instructionDecoder = Module(new InstructionDecoder(2))

      private val aluExecuteQueue =
        Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu))
      private val alu = Module(new ALU())

      private val aluExecuteQueue2 =
        Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu2))
      private val alu2 = Module(new ALU())

      private val broadcast = Module(new PriorityBroadcaster(2))

      private val retireMock = Module(new RetireMock())

      aluExecuteQueue.io.broadcast <> broadcast.io.broadcast
      aluExecuteQueue2.io.broadcast <> broadcast.io.broadcast

      alu.io.in <> aluExecuteQueue.io.deq
      alu2.io.in <> aluExecuteQueue2.io.deq

      broadcast.io.queues(0) <> alu.io.out
      broadcast.io.queues(1) <> alu2.io.out

      instructionDecoder.io.tableWrite <> rob.io.tableWrite
      broadcast.io.tableCommit <> rob.io.tableCommit

      registerMappingTable.io.mapping <> instructionDecoder.io.mapping
      registerMappingTable.io.alloc <> rob.io.alloc
      registerMappingTable.io.broadcast <> broadcast.io.broadcast
      rob.io.retired <> retireMock.io.retired

      instructionDecoder.io.broadcast <> broadcast.io.broadcast

      instructionDecoder.io.in <> io.in
      instructionDecoder.io.enqs(0) <> aluExecuteQueue.io.enqAndType
      instructionDecoder.io.enqs(1) <> aluExecuteQueue2.io.enqAndType

      retireMock.io.tableRetire <> rob.io.tableRetire
    }).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rs2 rs1 rd

      dut.io.in
        .bits(0)
        .pc
        .poke(1.U)
      dut.io.in
        .bits(0)
        .instruction
        .poke("b0000000_00001_00001_000_00001_0010011".U)
      dut.io.in
        .bits(0)
        .valid
        .poke(true.B)

      dut.io.in
        .bits(1)
        .pc
        .poke(2.U)
      dut.io.in
        .bits(1)
        .instruction
        .poke("b0000000_00001_00001_000_00001_0110011".U)
      dut.io.in
        .bits(1)
        .valid
        .poke(true.B)

      dut.io.in.valid.poke(true.B)

      dut.clock.step(70)
    }
  }
}
