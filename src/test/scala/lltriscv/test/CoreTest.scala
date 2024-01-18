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
import lltriscv.core.interconnect.SMAWithStoreQueueInterconnect
import lltriscv.bus.SMAReaderIO
import lltriscv.bus.SMAWriterIO
import lltriscv.test.mock.FlatMemoryMock
import lltriscv.test.mock.MemoryFileMock
import lltriscv.utils.ChiselUtils

class CoreTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(2, new PCVerifyStageEntry())))
    val pc = Output(DataType.address)

    val smaReader = new SMAReaderIO()
    val smaWriter = new SMAWriterIO()
  })
  private val fetcher = Module(new PCVerifyStage())
  private val registerMappingTable = Module(new RegisterMappingTable())
  private val rob = Module(new ROB(8))
  private val instructionDecoder = Module(new InstructionDecoder(3))

  private val aluExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.alu))
  private val alu = Module(new ALU())

  private val branchExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.branch))
  private val branch = Module(new Branch())

  private val memoryExecuteQueue =
    Module(new InOrderedExecuteQueue(8, ExecuteQueueType.memory))
  private val memory = Module(new Memory())

  private val broadcast = Module(new RoundRobinBroadcaster(3))

  private val retireMock = Module(new InstructionRetire(8))

  private val csr = Module(new CSRs())

  private val storeQueue = Module(new StoreQueue(8))
  private val storeQueueMemoryWriter = Module(new StoreQueueMemoryWriter())
  private val smaWithStoreQueueInterconnect = Module(new SMAWithStoreQueueInterconnect())

  fetcher.io.in <> io.in
  io.pc := fetcher.io.pc

  aluExecuteQueue.io.broadcast <> broadcast.io.broadcast
  branchExecuteQueue.io.broadcast <> broadcast.io.broadcast
  memoryExecuteQueue.io.broadcast <> broadcast.io.broadcast

  alu.io.in <> aluExecuteQueue.io.deq
  branch.io.in <> branchExecuteQueue.io.deq
  memory.io.in <> memoryExecuteQueue.io.deq

  broadcast.io.queues(0) <> alu.io.out
  broadcast.io.queues(1) <> branch.io.out
  broadcast.io.queues(2) <> memory.io.out

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
  instructionDecoder.io.enqs(2) <> memoryExecuteQueue.io.enqAndType

  retireMock.io.tableRetire <> rob.io.tableRetire

  rob.io.recover := retireMock.io.recover
  instructionDecoder.io.recover := retireMock.io.recover
  aluExecuteQueue.io.recover := retireMock.io.recover
  branchExecuteQueue.io.recover := retireMock.io.recover
  memoryExecuteQueue.io.recover := retireMock.io.recover
  alu.io.recover := retireMock.io.recover
  branch.io.recover := retireMock.io.recover
  memory.io.recover := retireMock.io.recover
  storeQueue.io.recover := retireMock.io.recover
  fetcher.io.recover := retireMock.io.recover
  fetcher.io.correctPC := retireMock.io.correctPC

  alu.io.csr <> csr.io.read
  alu.io.privilege := csr.io.privilege

  retireMock.io.csr <> csr.io.write
  retireMock.io.exception <> csr.io.exception
  retireMock.io.store <> storeQueue.io.retire

  memory.io.alloc <> storeQueue.io.alloc

  memory.io.sma <> smaWithStoreQueueInterconnect.io.in
  smaWithStoreQueueInterconnect.io.bypass <> storeQueue.io.bypass

  smaWithStoreQueueInterconnect.io.out <> io.smaReader

  storeQueueMemoryWriter.io.deq <> storeQueue.io.deq
  storeQueueMemoryWriter.io.sma <> io.smaWriter
}

class RegisterMappingTableTest extends AnyFreeSpec with ChiselScalatestTester {
  // "Print verilog" in {
  //   emitVerilog(new CoreTest(), Array("--target-dir", "generated"))
  // }

  "RegisterMappingTable should be OK" in {
    test(new CoreTest()).withAnnotations(
      Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    ) { dut =>
      val memory = new FlatMemoryMock(1024) with MemoryFileMock
      memory.importBin(new File("test.bin"), 0)

      println(memory.loadInt(0))
      for (j <- 0 to 1000) {
        println(j)
        for (i <- 0 until 2) {
          dut.io.in
            .bits(i)
            .instruction
            .poke(ChiselUtils.int2UInt(memory.loadInt((dut.io.pc.peekInt().toInt + 4 * i))))

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

        // Read write memory
        dut.io.smaReader.ready.poke(false.B)
        dut.io.smaReader.error.poke(false.B)
        dut.io.smaWriter.ready.poke(false.B)
        dut.io.smaWriter.error.poke(false.B)
        if (dut.io.smaReader.valid.peekBoolean()) {
          dut.io.smaReader.ready.poke(true.B)
          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.byte) {
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadByte(dut.io.smaReader.address.peekInt().toInt)))
          }

          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.half) {
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadShort(dut.io.smaReader.address.peekInt().toInt)))
          }

          if (dut.io.smaReader.readType.peek() == MemoryAccessLength.word) {
            val addr = dut.io.smaReader.address.peekInt()
            println(s"Reader addr: ${addr}")
            dut.io.smaReader.data.poke(ChiselUtils.int2UInt(memory.loadInt(dut.io.smaReader.address.peekInt().toInt)))
          }
        } else if (dut.io.smaWriter.valid.peekBoolean()) {
          dut.io.smaWriter.ready.poke(true.B)

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.byte) {
            memory.storeByte(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()).toByte)
          }

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.half) {
            memory.storeShort(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()).toShort)
          }

          if (dut.io.smaWriter.writeType.peek() == MemoryAccessLength.word) {
            memory.storeInt(dut.io.smaWriter.address.peekInt().toInt, ChiselUtils.BigInt2Int(dut.io.smaWriter.data.peekInt()))
          }
        }
        dut.clock.step()
      }
    }
  }
}
