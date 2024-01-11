package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType

/** The entry represents one instruction that has been fetched by fetcher.
  */
class FetchedInstructionEntry extends Bundle {
  // Instruction
  val instruction = DataType.instructionType.cloneType
  // Corresponding PC
  val pc = DataType.pcType.cloneType
  // Validity
  val valid = Bool()
}

/** The fetcher interface with instruction cache.
  *
  * fetchWidth: The maximum throughput of cache.
  */
class InstructionFetcherIO(fetchWidth: Int) extends Bundle {
  require(fetchWidth > 0)
  // PC port(write)
  val pc = new DecoupledIO(DataType.pcType)
  // Instruction port(read)
  val instruction = Flipped(
    new DecoupledIO(Vec(fetchWidth, new FetchedInstructionEntry()))
  )
}

/** The fetcher datapath.
  *
  * fetchWidth: The maximum throughput of cache.
  */
class InstructionFetcherDatapath(fetchWidth: Int) extends Module {
  require(fetchWidth > 0)
  val io = IO(new Bundle {
    // Cache interface
    val fetcher = new InstructionFetcherIO(fetchWidth)

    // Instruction FIFO enqueue interface
    val instructionFifo =
      new DecoupledIO(Vec(fetchWidth, new FetchedInstructionEntry()))

    // PC to be obtained
    val pc = Input(DataType.pcType)

    // Asserted when instruction has been queued
    val pcReady = Output(Bool())

    // How many valid instructions were obtained
    val fetchNumber = Output(UInt(unsignedBitLength(fetchWidth).W))
  })

  // PC is always valid
  io.fetcher.pc.valid := true.B
  io.fetcher.pc.bits := io.pc

  // Cache -> Fetcher -> Instruction FIFO
  io.instructionFifo <> io.fetcher.instruction

  // Logic: pcReady
  when(io.fetcher.instruction.valid && io.fetcher.instruction.ready) {
    io.pcReady := true.B

    // Counting the number of valid instructions
    io.fetchNumber := io.fetcher.instruction.bits.foldLeft(
      0.U(unsignedBitLength(fetchWidth).W)
    )((a, b) => a + Mux(b.valid, 1.U, 0.U))
  }.otherwise {
    io.pcReady := false.B
    io.fetchNumber := 0.U
  }
}

/** The PC Controller FSM.
  *
  * The generation of PC is related to the number of instructions obtained last
  * time and jump instructions.
  *
  * By default, PC = PC + fetchNumber
  *
  * fetchWidth: The maximum throughput of cache.
  */
class PcController(fetchWidth: Int) extends Module {
  val io = IO(new Bundle {
    // PC output
    val pc = Output(DataType.pcType)

    // Asserted when instruction has been queued
    val pcReady = Input(Bool())

    // How many valid instructions were obtained
    val fetchNumber = Input(UInt(unsignedBitLength(fetchWidth).W))
  })

  val pcReg = RegInit(0.U.asTypeOf(DataType.pcType))
  when(io.pcReady) {
    pcReg := pcReg + io.fetchNumber
  }

  io.pc := pcReg
}

/** The instruction fetcher.
  *
  * fetchWidth: The maximum throughput of cache.
  *
  * fifoLength: Internal instruction FIFO length.
  */
class InstructionFetcher(fetchWidth: Int, fifoLength: Int) extends Module {
  val io = IO(new Bundle {
    // Cache interface
    val fetcher = new InstructionFetcherIO(fetchWidth)

    // Instruction FIFO dequeue interface
    val puller =
      new DecoupledIO(Vec(fetchWidth, new FetchedInstructionEntry()))
  })

  private val instructionFetcherDatapath = Module(
    new InstructionFetcherDatapath(fetchWidth)
  )

  instructionFetcherDatapath.io.fetcher <> io.fetcher

  // Instruction FIFO implemented by Chisel3 Queue
  private val instructionFifo = Module(
    new Queue(Vec(fetchWidth, new FetchedInstructionEntry()), fifoLength)
  )

  instructionFetcherDatapath.io.instructionFifo <> instructionFifo.io.enq
  io.puller <> instructionFifo.io.deq

  private val pcController = Module(new PcController(fetchWidth))

  pcController.io.fetchNumber := instructionFetcherDatapath.io.fetchNumber
  pcController.io.pcReady := instructionFetcherDatapath.io.pcReady
  instructionFetcherDatapath.io.pc := pcController.io.pc
}
