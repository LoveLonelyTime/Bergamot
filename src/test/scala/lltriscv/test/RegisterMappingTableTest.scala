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
    when(
      io.tableRetire.entries(id1).valid || io.tableRetire.entries(id2).valid
    ) {
      printf(
        "retired instruction: \n pc = %d , r = %d, v = %d \n pc = %d , r = %d, v = %d \n",
        io.tableRetire.entries(id1).pc,
        io.tableRetire.entries(id1).result,
        io.tableRetire.entries(id1).valid,
        io.tableRetire.entries(id2).pc,
        io.tableRetire.entries(id2).result,
        io.tableRetire.entries(id2).valid
      )

      when(
        io.tableRetire.entries(id1).valid &&
          io.tableRetire.entries(id1).real =/= io.tableRetire.entries(id1).spec
      ) {
        printf(
          "spec violate!!!: pc = %d, sepc = %d, real = %d\n",
          io.tableRetire.entries(id1).pc,
          io.tableRetire.entries(id1).spec,
          io.tableRetire.entries(id1).real
        )
      }

      when(
        io.tableRetire.entries(id2).valid &&
          io.tableRetire.entries(id2).real =/= io.tableRetire.entries(id2).spec
      ) {
        printf(
          "spec violate!!!: pc = %d, sepc = %d, real = %d\n",
          io.tableRetire.entries(id2).pc,
          io.tableRetire.entries(id2).spec,
          io.tableRetire.entries(id2).real
        )
      }
    }
  }
}

class CoreTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new DecodeStageEntry())))
  })
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

  private val retireMock = Module(new RetireMock())

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
  rob.io.retired <> retireMock.io.retired

  instructionDecoder.io.broadcast <> broadcast.io.broadcast

  instructionDecoder.io.in <> io.in
  instructionDecoder.io.enqs(0) <> aluExecuteQueue.io.enqAndType
  instructionDecoder.io.enqs(1) <> aluExecuteQueue2.io.enqAndType
  instructionDecoder.io.enqs(2) <> branchExecuteQueue.io.enqAndType

  retireMock.io.tableRetire <> rob.io.tableRetire
}

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  "Print verilog" in {
    emitVerilog(new CoreTest(), Array("--target-dir", "generated"))
  }

  "RegisterMappingTable should be OK" in {
    test(new CoreTest()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rs2 rs1 rd
      var __pc = 0
      def apc = {
        __pc = __pc + 1
        __pc
      }

      def toBinaryString(x: Int, width: Int) =
        String
          .format("%" + width + "s", Integer.toBinaryString(x))
          .replace(' ', '0')

      def add(rs1: Int, rs2: Int, rd: Int, pc: Int) =
        (
          s"b0000000_${toBinaryString(rs2, 5)}_${toBinaryString(rs1, 5)}_000_${toBinaryString(rd, 5)}_0110011".U,
          pc.U(32.W),
          (pc + 4).U(32.W),
          (pc + 4).U(32.W),
          true.B
        )

      def addi(rs1: Int, imm: Int, rd: Int, pc: Int) =
        (
          s"b${toBinaryString(imm, 12)}_${toBinaryString(rs1, 5)}_000_${toBinaryString(rd, 5)}_0010011".U,
          pc.U(32.W),
          (pc + 4).U(32.W),
          (pc + 4).U(32.W),
          true.B
        )

      def bne(rs1: Int, rs2: Int, pc: Int) =
        (
          s"b0000000_${toBinaryString(rs2, 5)}_${toBinaryString(rs1, 5)}_001_00110_1100011".U,
          pc.U(32.W),
          (pc + 4).U(32.W),
          (pc + 4).U(32.W),
          true.B
        )

      def nop() = (
        0.U(32.W),
        0.U(32.W),
        0.U(32.W),
        0.U(32.W),
        false.B
      )

      def pushInstruction(
          instr1: UInt,
          pc1: UInt,
          next1: UInt,
          spec1: UInt,
          valid1: Bool,
          instr2: UInt,
          pc2: UInt,
          next2: UInt,
          spec2: UInt,
          valid2: Bool
      ) = {
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.foreach(_.valid.poke(false.B))
        while (!dut.io.in.ready.peekBoolean()) dut.clock.step()
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits(0).pc.poke(pc1)
        dut.io.in.bits(0).spec.poke(spec1)
        dut.io.in.bits(0).next.poke(next1)
        dut.io.in.bits(0).instruction.poke(instr1)
        dut.io.in.bits(0).valid.poke(valid1)

        dut.io.in.bits(1).pc.poke(pc2)
        dut.io.in.bits(1).spec.poke(spec2)
        dut.io.in.bits(1).next.poke(next2)
        dut.io.in.bits(1).instruction.poke(instr2)
        dut.io.in.bits(1).valid.poke(valid2)

        dut.clock.step()
        dut.io.in.bits.foreach(_.valid.poke(false.B))
        dut.io.in.valid.poke(false.B)
      }

      // val instrs = List(
      //   addi(0, 1, 1, apc),
      //   add(1, 1, 1, apc),
      //   add(1, 1, 1, apc),
      //   addi(1, 1, 2, apc),
      //   add(2, 2, 2, apc),
      //   add(2, 2, 2, apc),
      //   bne(1, 2, apc)
      // )

      val instrs = List(
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc),
        addi(0, 1, 1, apc)
      )

      instrs
        .grouped(2)
        .foreach(grp => {
          if (grp.length == 2) {
            pushInstruction(
              grp(0)._1,
              grp(0)._2,
              grp(0)._3,
              grp(0)._4,
              grp(0)._5,
              grp(1)._1,
              grp(1)._2,
              grp(1)._3,
              grp(1)._4,
              grp(1)._5
            )
          } else if (grp.length == 1) {
            val np = nop()
            pushInstruction(
              grp(0)._1,
              grp(0)._2,
              grp(0)._3,
              grp(0)._4,
              grp(0)._5,
              np._1,
              np._2,
              np._3,
              np._4,
              np._5
            )
          }
        })
      dut.clock.step(50)
    }
  }
}
