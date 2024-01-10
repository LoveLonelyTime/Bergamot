package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core.broadcast.DataBroadcastIO

class ExecuteQueue(depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new ExecuteEntry()))
    val deq = DecoupledIO(new ExecuteEntry())
    val broadcast = Flipped(new DataBroadcastIO())
  })
}

class InOrderExecuteQueue(depth: Int) extends ExecuteQueue(depth) {
  private val queue = Reg(Vec(depth, new ExecuteQueueEntry()))

  def counter(depth: Int, incr: Bool): (UInt, UInt) = {
    val cntReg = RegInit(0.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
    when(incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)

  io.enq.ready := !fullReg

  // Inorder arbitration logic
  io.deq.valid := !emptyReg &&
    !queue(readPtr).rs1.pending &&
    !queue(readPtr).rs2.pending

  io.deq.bits := queue(readPtr)

  private val op =
    (io.enq.valid && io.enq.ready) ## (io.deq.valid && io.deq.ready)
  private val doWrite = WireDefault(false.B)

  switch(op) {
    is("b01".U) { // read
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
    }
    is("b10".U) { // write
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
      doWrite := true.B
    }

    is("b11".U) { // read and write
      incrRead := true.B
      incrWrite := true.B
      doWrite := true.B
    }
  }

  // Broadcast logic
  for (
    i <- 0 until depth;
    j <- 0 until 2
  ) {
    when(
      queue(i).rs1.pending &&
        io.broadcast.entries(j).valid &&
        queue(i).rs1.receipt === io.broadcast.entries(j).receipt
    ) { // rs1
      queue(i).rs1.pending := false.B
      queue(i).rs1.receipt := io.broadcast.entries(j).data
    }

    when(
      queue(i).rs2.pending &&
        io.broadcast.entries(j).valid &&
        queue(i).rs2.receipt === io.broadcast.entries(j).receipt
    ) { // rs2
      queue(i).rs2.pending := false.B
      queue(i).rs2.receipt := io.broadcast.entries(j).data
    }
  }

  // Write logic
  when(doWrite) {
    queue(writePtr) := io.enq.bits
  }
}
