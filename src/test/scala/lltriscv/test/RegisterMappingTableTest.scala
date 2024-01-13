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
import lltriscv.core.broadcast.RoundRobinBroadcaster
import lltriscv.core.retire.InstructionRetire
import lltriscv.core.fetch.PCVerifyStage
import lltriscv.core.fetch.PCVerifyStageEntry

class CoreTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new PCVerifyStageEntry())))
    val pc = Output(DataType.pc)
  })
  private val fetcher = Module(new PCVerifyStage())
  private val registerMappingTable = Module(new RegisterMappingTable())
  private val rob = Module(new ROB(8))
  private val instructionDecoder = Module(new InstructionDecoder(3))

  private val aluExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu))
  private val alu = Module(new ALU())

  private val aluExecuteQueue2 =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu2))
  private val alu2 = Module(new ALU())

  private val branchExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.branch))
  private val branch = Module(new Branch())

  private val broadcast = Module(new RoundRobinBroadcaster(3))

  private val retireMock = Module(new InstructionRetire(8))

  fetcher.io.in <> io.in
  io.pc := fetcher.io.pc

  aluExecuteQueue.io.broadcast <> broadcast.io.broadcast
  aluExecuteQueue2.io.broadcast <> broadcast.io.broadcast
  branchExecuteQueue.io.broadcast <> broadcast.io.broadcast

  alu.io.in <> aluExecuteQueue.io.deq
  alu2.io.in <> aluExecuteQueue2.io.deq
  branch.io.in <> branchExecuteQueue.io.deq

  broadcast.io.queues(0) <> alu.io.out
  broadcast.io.queues(1) <> alu2.io.out
  broadcast.io.queues(2) <> branch.io.out

  instructionDecoder.io.tableWrite <> rob.io.tableWrite
  broadcast.io.tableCommit <> rob.io.tableCommit

  registerMappingTable.io.mapping <> instructionDecoder.io.mapping
  registerMappingTable.io.alloc <> rob.io.alloc
  registerMappingTable.io.broadcast <> broadcast.io.broadcast
  registerMappingTable.io.update <> retireMock.io.update
  registerMappingTable.io.recover <> retireMock.io.recover
  rob.io.retired <> retireMock.io.retired

  instructionDecoder.io.broadcast <> broadcast.io.broadcast

  instructionDecoder.io.in <> fetcher.io.out
  instructionDecoder.io.enqs(0) <> aluExecuteQueue.io.enqAndType
  instructionDecoder.io.enqs(1) <> aluExecuteQueue2.io.enqAndType
  instructionDecoder.io.enqs(2) <> branchExecuteQueue.io.enqAndType

  retireMock.io.tableRetire <> rob.io.tableRetire

  rob.io.recover := retireMock.io.recover
  instructionDecoder.io.recover := retireMock.io.recover
  aluExecuteQueue.io.recover := retireMock.io.recover
  aluExecuteQueue2.io.recover := retireMock.io.recover
  branchExecuteQueue.io.recover := retireMock.io.recover
  alu.io.recover := retireMock.io.recover
  alu2.io.recover := retireMock.io.recover
  branch.io.recover := retireMock.io.recover

  fetcher.io.recover := retireMock.io.recover
  fetcher.io.correctPC := retireMock.io.correctPC
}

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "Print verilog" in {
    emitVerilog(new CoreTest(), Array("--target-dir", "generated"))
  }

  "RegisterMappingTable should be OK" in {
    test(new CoreTest()).withAnnotations(
      Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    ) { dut =>
      val memory = Array.ofDim[UInt](64)

      def toBinaryString(x: Int, width: Int) =
        String
          .format("%" + width + "s", Integer.toBinaryString(x))
          .replace(' ', '0')

      def add(rs1: Int, rs2: Int, rd: Int) =
        s"b0000000_${toBinaryString(rs2, 5)}_${toBinaryString(rs1, 5)}_000_${toBinaryString(rd, 5)}_0110011".U

      def addi(rs1: Int, imm: Int, rd: Int) =
        s"b${toBinaryString(imm, 12)}_${toBinaryString(rs1, 5)}_000_${toBinaryString(rd, 5)}_0010011".U

      def bne(rs1: Int, rs2: Int) =
        s"b0000000_${toBinaryString(rs2, 5)}_${toBinaryString(rs1, 5)}_001_01000_1100011".U

      for (i <- 0 until memory.length) memory(i) = 0.U(32.W)
      memory(0) = addi(0, 1, 1)
      memory(1) = add(1, 1, 1)
      memory(2) = bne(0, 1)
      memory(3) = add(1, 1, 1) // skip
      memory(4) = addi(1, 5, 1)

      def get_mem(x: Int) = {
        if (x < memory.length) {
          memory(x)
        } else {
          0.U(32.W)
        }
      }

      for (j <- 0 to 100) {
        for (i <- 0 until 2) {
          dut.io.in
            .bits(i)
            .instruction
            .poke(get_mem((dut.io.pc.peekInt() / 4 + i).toInt))
          dut.io.in
            .bits(i)
            .pc
            .poke((dut.io.pc.peekInt() + 4 * i).U)

          dut.io.in
            .bits(i)
            .next
            .poke(
              (dut.io.pc.peekInt() + 4 * (i + 1)).U
            )
          dut.io.in
            .bits(i)
            .spec
            .poke(
              (dut.io.pc.peekInt() + 4 * (i + 1)).U
            )
          dut.io.in.bits(i).valid.poke(true.B)
        }
        dut.io.in.valid.poke(true.B)
        dut.clock.step()
      }
    }
  }
}
