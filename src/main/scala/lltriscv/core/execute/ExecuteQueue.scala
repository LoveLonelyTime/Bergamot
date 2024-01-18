package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core.broadcast.DataBroadcastIO
import lltriscv.utils.CoreUtils

/*
 * Execute queue (aka reservation station)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** The abstract class ExecuteQueue
  *
  * Describe the basic interface
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  */
abstract class ExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type) extends Module {
  require(depth > 0, "Execute queue depth must be greater than 0")

  val io = IO(new Bundle {
    // Enq and deq interface
    val enqAndType = Flipped(new ExecuteQueueEnqueueIO())
    val deq = DecoupledIO(new ExecuteEntry())

    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())

    // Recovery interface
    val recover = Input(Bool())
  })

  // Hardwired
  io.enqAndType.queueType := queueType
}

/** The in ordered implementation of execute queue
  *
  * This execute queue is in ordered, which is suitable for instruction sets that require in ordered within the queue
  *
  * Implementation using read/write pointer queue
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  */
class InOrderedExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type) extends ExecuteQueue(depth, queueType) {

  // Read/Wirte pointers
  private val queue = Reg(Vec(depth, new ExecuteEntry()))

  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = CoreUtils.pointer(depth, incrRead)
  private val (writePtr, nextWrite) = CoreUtils.pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.enqAndType.enq.ready := !fullReg

  // In ordered arbitration logic
  io.deq.valid := !emptyReg && // Not empty
    (
      (!queue(readPtr).rs1.pending &&
        !queue(readPtr).rs2.pending) || // Operands are ready
        !queue(readPtr).valid // Or invalid
    )

  io.deq.bits := queue(readPtr)

  // Queue logic
  private val op = (io.enqAndType.enq.valid && io.enqAndType.enq.ready) ##
    (io.deq.valid && io.deq.ready)
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
    // rs1 and rs2
    CoreUtils.matchBroadcast(queue(i).rs1, io.broadcast.entries(j))
    CoreUtils.matchBroadcast(queue(i).rs2, io.broadcast.entries(j))
  }

  // Write logic
  when(doWrite) {
    queue(writePtr) := io.enqAndType.enq.bits
  }

  // Recovery logic
  when(io.recover) {
    queue.foreach(_.valid := false.B)
  }
}
