package lltriscv.core

import chisel3._
import chisel3.util._
import lltriscv.core.fetch.Fetch
import lltriscv.core.decode.Decode
import lltriscv.core.record.TLB
import lltriscv.cache.TrivialICache
import lltriscv.core.record.RegisterMappingTable
import lltriscv.core.broadcast.DataBroadcastIO
import lltriscv.core.execute.ExecuteQueueEnqueueIO
import lltriscv.core.record.ROBTableWriteIO
import lltriscv.bus.SMAReaderIO
import lltriscv.interconnect.SMA2ReaderInterconnect
import lltriscv.core.record.PrivilegeType
import lltriscv.cache.FlushCacheIO
import lltriscv.core.record.RegisterUpdateIO
import lltriscv.core.execute.InOrderedExecuteQueue
import lltriscv.core.execute.ExecuteQueueType
import lltriscv.core.execute.Branch
import lltriscv.core.execute.Memory
import lltriscv.core.execute.ALU
import lltriscv.core.record.CSRsReadIO
import lltriscv.cache.TrivialDCache
import lltriscv.cache.Serial2Flusher
import lltriscv.bus.SMAWriterIO
import lltriscv.core.record.StoreQueue
import lltriscv.core.record.StoreQueueMemoryWriter
import lltriscv.interconnect.SMAWithStoreQueueInterconnect
import lltriscv.core.record.StoreQueueRetireIO
import lltriscv.core.execute.ExecuteResultEntry
import lltriscv.core.broadcast.RoundRobinBroadcaster
import lltriscv.core.retire.InstructionRetire
import lltriscv.core.record.CSRs
import lltriscv.core.record.ROB
import decode.Decode
import lltriscv.cache.Parallel2Flusher
import lltriscv.core.execute.OutOfOrderedExecuteQueue
import lltriscv.core.fetch.BranchPredictorUpdateIO

/*
 * LLT RISC-V Core Exquisite integration
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Core config class
  *
  * @param iTLBDepth
  * @param iCacheLineDepth
  * @param fetchQueueDepth
  * @param executeQueueWidth
  * @param executeQueueDepth
  * @param dTLBDepth
  * @param storeQueueDepth
  * @param robDepth
  * @param predictorDepth
  * @param pcInit
  */
case class CoreConfig(val iTLBDepth: Int, val iCacheLineDepth: Int, val fetchQueueDepth: Int, val executeQueueWidth: Int, val executeQueueDepth: Int, val dTLBDepth: Int, val storeQueueDepth: Int, val robDepth: Int, val predictorDepth: Int, pcInit: Int)

object CoreConfig {

  /** Default config
    */
  val default = CoreConfig(
    iTLBDepth = 8,
    iCacheLineDepth = 8,
    fetchQueueDepth = 8,
    executeQueueWidth = 3,
    executeQueueDepth = 8,
    dTLBDepth = 8,
    storeQueueDepth = 8,
    robDepth = 8,
    predictorDepth = 8,
    pcInit = 0x0
  )
}

/** LLTRISCVCoreExq
  *
  * @param config
  *   Core config
  */
class LLTRISCVCoreExq(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val smaReader = new SMAReaderIO()
    val smaWriter = new SMAWriterIO()
  })

  private val coreFrontend = Module(new CoreFrontend(config))
  private val coreExecute = Module(new CoreExecute(config))
  private val coreBackend = Module(new CoreBackend(config))

  private val reader2Interconnect = Module(new SMA2ReaderInterconnect())
  private val tlbFlusher = Module(new Parallel2Flusher())

  // CoreFrontend
  coreFrontend.io.sma <> reader2Interconnect.io.in1
  coreFrontend.io.broadcast <> coreBackend.io.broadcast
  coreFrontend.io.alloc <> coreBackend.io.alloc
  coreFrontend.io.tableWrite <> coreBackend.io.tableWrite
  coreFrontend.io.correctPC := coreBackend.io.correctPC
  coreFrontend.io.recover := coreBackend.io.recover
  coreFrontend.io.privilege := coreBackend.io.privilege
  coreFrontend.io.mstatus := coreBackend.io.mstatus
  coreFrontend.io.satp := coreBackend.io.satp

  // CoreExecute
  coreExecute.io.enqs <> coreFrontend.io.enqs
  coreExecute.io.csr <> coreBackend.io.read
  coreExecute.io.broadcast <> coreBackend.io.broadcast
  coreExecute.io.smaReader <> reader2Interconnect.io.in2
  coreExecute.io.smaWriter <> io.smaWriter
  coreExecute.io.privilege := coreBackend.io.privilege
  coreExecute.io.mstatus := coreBackend.io.mstatus
  coreExecute.io.satp := coreBackend.io.satp
  coreExecute.io.recover := coreBackend.io.recover

  // CoreBackend
  coreBackend.io.deqs <> coreExecute.io.deqs
  coreBackend.io.dCacheFlush <> coreExecute.io.dCacheFlush
  coreBackend.io.iCacheFlush <> coreFrontend.io.iCacheFlush
  coreBackend.io.iCacheFlush.empty := true.B
  coreBackend.io.tlbFlush <> tlbFlusher.io.in
  coreBackend.io.update <> coreFrontend.io.update
  coreBackend.io.predictorUpdate <> coreFrontend.io.predictorUpdate
  coreBackend.io.store <> coreExecute.io.retire

  // TLBFlusher
  tlbFlusher.io.out1 <> coreFrontend.io.iTLBFlush
  tlbFlusher.io.out2 <> coreExecute.io.dTLBFlush

  // Reader2Interconnect
  reader2Interconnect.io.out <> io.smaReader
}

/** Core frontend
  *
  * From instruction fetch to dispatching
  *
  * @param config
  *   Core config
  */
class CoreFrontend(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val sma = new SMAReaderIO()
    val broadcast = Flipped(new DataBroadcastIO())
    val enqs = Vec(config.executeQueueWidth, new ExecuteQueueEnqueueIO())

    val alloc = Flipped(DecoupledIO(DataType.receipt))
    val tableWrite = new ROBTableWriteIO()
    val update = Flipped(new RegisterUpdateIO())
    val predictorUpdate = Flipped(new BranchPredictorUpdateIO())

    val correctPC = Input(DataType.address)
    val recover = Input(Bool())

    val iTLBFlush = Flipped(new FlushCacheIO())
    val iCacheFlush = Flipped(new FlushCacheIO())

    val privilege = Input(PrivilegeType())
    val satp = Input(DataType.operation)
    val mstatus = Input(DataType.operation)
  })

  private val itlb = Module(new TLB(config.iTLBDepth, false))
  private val iCache = Module(new TrivialICache(config.iCacheLineDepth))
  private val fetch = Module(new Fetch(config.iCacheLineDepth, config.fetchQueueDepth, config.predictorDepth, config.pcInit))
  private val decode = Module(new Decode(config.executeQueueWidth))
  private val registerMappingTable = Module(new RegisterMappingTable())

  private val reader2Interconnect = Module(new SMA2ReaderInterconnect())

  // ITLB
  itlb.io.sma <> reader2Interconnect.io.in1
  itlb.io.privilege := io.privilege
  itlb.io.satp := io.satp
  itlb.io.mstatus := io.mstatus
  itlb.io.flush <> io.iTLBFlush

  // ICache
  iCache.io.downReader <> reader2Interconnect.io.in2
  iCache.io.flush <> io.iCacheFlush

  // Fetch
  fetch.io.satp := io.satp
  fetch.io.update <> io.predictorUpdate
  fetch.io.itlb <> itlb.io.request
  fetch.io.icache <> iCache.io.request
  fetch.io.recover := io.recover
  fetch.io.correctPC := io.correctPC
  fetch.io.out <> decode.io.in

  // Decode
  decode.io.mapping <> registerMappingTable.io.mapping
  decode.io.broadcast <> io.broadcast
  decode.io.tableWrite <> io.tableWrite
  decode.io.recover := io.recover
  decode.io.enqs <> io.enqs

  // RegisterMappingTable
  registerMappingTable.io.alloc <> io.alloc
  registerMappingTable.io.broadcast <> io.broadcast
  registerMappingTable.io.update <> io.update
  registerMappingTable.io.recover <> io.recover

  // Reader2Interconnect
  reader2Interconnect.io.out <> io.sma
}

/** Core execute
  *
  * Including three instruction cores: ALU, Memory, Branch
  *
  * @param config
  *   Core config
  */
class CoreExecute(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val enqs = Flipped(Vec(config.executeQueueWidth, new ExecuteQueueEnqueueIO()))
    val deqs = Vec(config.executeQueueWidth, DecoupledIO(new ExecuteResultEntry()))

    val csr = Flipped(new CSRsReadIO())
    val broadcast = Flipped(new DataBroadcastIO())

    val smaReader = new SMAReaderIO()
    val smaWriter = new SMAWriterIO()

    val privilege = Input(PrivilegeType())
    val satp = Input(DataType.operation)
    val mstatus = Input(DataType.operation)

    val dTLBFlush = Flipped(new FlushCacheIO())
    val dCacheFlush = Flipped(new FlushCacheIO())

    val retire = Flipped(new StoreQueueRetireIO())

    val recover = Input(Bool())

  })

  private val aluExecuteQueue =
    Module(new OutOfOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.alu))
  private val alu = Module(new ALU())

  private val branchExecuteQueue =
    Module(new OutOfOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.branch))
  private val branch = Module(new Branch())

  private val memoryExecuteQueue =
    Module(new InOrderedExecuteQueue(config.executeQueueDepth, ExecuteQueueType.memory))
  private val memory = Module(new Memory())

  private val storeQueue = Module(new StoreQueue(config.storeQueueDepth))
  private val storeQueueMemoryWriter = Module(new StoreQueueMemoryWriter())
  private val smaWithStoreQueueInterconnect = Module(new SMAWithStoreQueueInterconnect())

  private val dtlb = Module(new TLB(config.dTLBDepth, true))
  private val dCache = Module(new TrivialDCache())
  private val reader2Interconnect = Module(new SMA2ReaderInterconnect())
  private val storeQueueAndDCacheFlusher = Module(new Serial2Flusher())

  // Enqueue
  io.enqs(0) <> aluExecuteQueue.io.enqAndType
  io.enqs(1) <> branchExecuteQueue.io.enqAndType
  io.enqs(2) <> memoryExecuteQueue.io.enqAndType

  // DTLB
  dtlb.io.sma <> reader2Interconnect.io.in1
  dtlb.io.privilege := io.privilege
  dtlb.io.satp := io.satp
  dtlb.io.mstatus := io.mstatus
  dtlb.io.flush <> io.dTLBFlush

  // DCache
  dCache.io.downReader <> io.smaReader
  dCache.io.downWriter <> io.smaWriter

  // StoreQueue
  storeQueue.io.deq <> storeQueueMemoryWriter.io.deq
  storeQueue.io.retire <> io.retire
  storeQueue.io.recover := io.recover

  // StoreQueueMemoryWriter
  storeQueueMemoryWriter.io.sma <> dCache.io.upWriter

  // SMAWithStoreQueueInterconnect
  smaWithStoreQueueInterconnect.io.bypass <> storeQueue.io.bypass
  smaWithStoreQueueInterconnect.io.out <> reader2Interconnect.io.in2

  // StoreQueueAndDCacheFlusher
  storeQueueAndDCacheFlusher.io.out1 <> storeQueue.io.flush
  storeQueueAndDCacheFlusher.io.out2 <> dCache.io.flush
  storeQueueAndDCacheFlusher.io.in <> io.dCacheFlush

  // ALU
  alu.io.in <> aluExecuteQueue.io.deq
  alu.io.csr <> io.csr
  alu.io.privilege := io.privilege
  alu.io.recover := io.recover
  aluExecuteQueue.io.broadcast <> io.broadcast
  aluExecuteQueue.io.recover := io.recover

  // Branch
  branch.io.in <> branchExecuteQueue.io.deq
  branch.io.recover := io.recover
  branchExecuteQueue.io.broadcast <> io.broadcast
  branchExecuteQueue.io.recover := io.recover

  // Memory
  memory.io.in <> memoryExecuteQueue.io.deq
  memory.io.dtlb <> dtlb.io.request
  memory.io.sma <> smaWithStoreQueueInterconnect.io.in
  memory.io.alloc <> storeQueue.io.alloc
  memory.io.recover := io.recover
  memoryExecuteQueue.io.broadcast <> io.broadcast
  memoryExecuteQueue.io.recover := io.recover

  // Reader2Interconnect
  reader2Interconnect.io.out <> dCache.io.upReader

  // Dequeue
  io.deqs(0) <> alu.io.out
  io.deqs(1) <> branch.io.out
  io.deqs(2) <> memory.io.out
}

/** Core backend
  *
  * Responsible for instructing retire and update core status
  *
  * @param config
  *   Core config
  */
class CoreBackend(config: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val deqs = Vec(config.executeQueueWidth, Flipped(DecoupledIO(new ExecuteResultEntry())))

    val alloc = DecoupledIO(DataType.receipt)
    val tableWrite = Flipped(new ROBTableWriteIO())

    val broadcast = new DataBroadcastIO()
    val update = new RegisterUpdateIO()
    val predictorUpdate = new BranchPredictorUpdateIO()
    val store = new StoreQueueRetireIO()
    val recover = Output(new Bool())
    val correctPC = Output(DataType.address)

    val read = new CSRsReadIO()
    val privilege = Output(PrivilegeType())
    val mstatus = Output(DataType.operation)
    val satp = Output(DataType.operation)

    val dCacheFlush = new FlushCacheIO()
    val iCacheFlush = new FlushCacheIO()
    val tlbFlush = new FlushCacheIO()
  })
  private val broadcaster = Module(new RoundRobinBroadcaster(config.executeQueueWidth))
  private val instructionRetire = Module(new InstructionRetire(config.robDepth))
  private val rob = Module(new ROB(config.robDepth))
  private val csr = Module(new CSRs())

  // Broadcaster
  broadcaster.io.queues <> io.deqs
  broadcaster.io.broadcast <> io.broadcast
  broadcaster.io.tableCommit <> rob.io.tableCommit

  // InstructionRetire
  instructionRetire.io.retired <> rob.io.retired
  instructionRetire.io.tableRetire <> rob.io.tableRetire
  instructionRetire.io.update <> io.update
  instructionRetire.io.predictorUpdate <> io.predictorUpdate
  instructionRetire.io.store <> io.store
  io.recover := instructionRetire.io.recover
  io.correctPC := instructionRetire.io.correctPC
  instructionRetire.io.csr <> csr.io.write
  instructionRetire.io.exception <> csr.io.exception
  instructionRetire.io.iCacheFlush <> io.iCacheFlush
  instructionRetire.io.dCacheFlush <> io.dCacheFlush
  instructionRetire.io.tlbFlush <> io.tlbFlush

  // ROB
  rob.io.alloc <> io.alloc
  rob.io.recover := instructionRetire.io.recover
  rob.io.tableWrite <> io.tableWrite

  // CSR
  csr.io.read <> io.read
  io.privilege := csr.io.privilege
  io.mstatus := csr.io.mstatus
  io.satp := csr.io.satp
}
