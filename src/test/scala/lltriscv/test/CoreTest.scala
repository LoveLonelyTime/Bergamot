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
import lltriscv.core.decode.InstructionDecoder
import lltriscv.core.broadcast.RoundRobinBroadcaster
import lltriscv.core.retire.InstructionRetire
import lltriscv.core.fetch.PCVerifyStage
import lltriscv.core.fetch.PCVerifyStageEntry
import java.io.File
import java.io.FileInputStream

class MemoryMock(len: Int) {
  private val memory = Array.ofDim[Byte](len)

  def toBinaryString(x: Int, width: Int) =
    String
      .format("%" + width + "s", Integer.toBinaryString(x))
      .replace(' ', '0')

  def importBin(file: File, start: Int) = {
    val in = new FileInputStream(file)
    var by = in.read()
    var id = 0
    while (by != -1) {
      memory(start + id) = by.toByte
      by = in.read()
      id = id + 1
    }
    in.close()
    println(s"Import from ${file.getName()} ${id} bytes.")
  }

  def readInt(start: Int) = {
    if (start + 3 < memory.length) {
      val a = memory(start).toInt & 0xff
      val b = memory(start + 1).toInt & 0xff
      val c = memory(start + 2).toInt & 0xff
      val d = memory(start + 3).toInt & 0xff
      val result = (d << 24) | (c << 16) | (b << 8) | a
      s"b${toBinaryString(result, 32)}".U
    } else {
      0.U
    }
  }
}

class CoreTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new PCVerifyStageEntry())))
    val pc = Output(DataType.address)
  })
  private val fetcher = Module(new PCVerifyStage())
  private val registerMappingTable = Module(new RegisterMappingTable())
  private val rob = Module(new ROB(8))
  private val instructionDecoder = Module(new InstructionDecoder(2))

  private val aluExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu))
  private val alu = Module(new ALU())

  private val branchExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.branch))
  private val branch = Module(new Branch())

  private val broadcast = Module(new RoundRobinBroadcaster(2))

  private val retireMock = Module(new InstructionRetire(8))

  private val csr = Module(new CSRs())

  fetcher.io.in <> io.in
  io.pc := fetcher.io.pc

  aluExecuteQueue.io.broadcast <> broadcast.io.broadcast
  branchExecuteQueue.io.broadcast <> broadcast.io.broadcast

  alu.io.in <> aluExecuteQueue.io.deq
  branch.io.in <> branchExecuteQueue.io.deq

  broadcast.io.queues(0) <> alu.io.out
  broadcast.io.queues(1) <> branch.io.out

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
  instructionDecoder.io.enqs(1) <> branchExecuteQueue.io.enqAndType

  retireMock.io.tableRetire <> rob.io.tableRetire

  rob.io.recover := retireMock.io.recover
  instructionDecoder.io.recover := retireMock.io.recover
  aluExecuteQueue.io.recover := retireMock.io.recover
  branchExecuteQueue.io.recover := retireMock.io.recover
  alu.io.recover := retireMock.io.recover
  branch.io.recover := retireMock.io.recover

  fetcher.io.recover := retireMock.io.recover
  fetcher.io.correctPC := retireMock.io.correctPC

  alu.io.csr <> csr.io.read
  alu.io.privilege := csr.io.privilege

  retireMock.io.csr <> csr.io.write
  retireMock.io.exception <> csr.io.exception
}

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  // "Print verilog" in {
  //   emitVerilog(new CoreTest(), Array("--target-dir", "generated"))
  // }

  "RegisterMappingTable should be OK" in {
    test(new CoreTest()).withAnnotations(
      Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    ) { dut =>
      val memory = new MemoryMock(64)
      memory.importBin(new File("test.bin"), 0)

      for (j <- 0 to 200) {
        for (i <- 0 until 2) {
          dut.io.in
            .bits(i)
            .instruction
            .poke(memory.readInt((dut.io.pc.peekInt().toInt + 4 * i)))

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
