package lltriscv.core.execute

import chisel3._
import chisel3.util._
import lltriscv.core.broadcast.DataBroadcastIO
import lltriscv.utils.CoreUtils
import lltriscv.utils.ChiselUtils._
import dataclass.data

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
  private val queue = RegInit(Vec(depth, new ExecuteEntry()).zero)

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
    CoreUtils.matchBroadcast(queue(i).rs1, queue(i).rs1, io.broadcast.entries(j))
    CoreUtils.matchBroadcast(queue(i).rs2, queue(i).rs2, io.broadcast.entries(j))
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

/** The in out of ordered implementation of execute queue
  *
  * Arbitration strategy is from old to new
  *
  * Implementation using double buffer queue
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  */
class OutOfOrderedExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type) extends ExecuteQueue(depth, queueType) {
  private class DoubleBuffer extends Module {
    val io = IO(new Bundle {
      val enq = Flipped(DecoupledIO(new ExecuteEntry()))
      val deq = DecoupledIO(new ExecuteEntry())
      val sideDeq = DecoupledIO(new ExecuteEntry())
      val broadcast = Flipped(new DataBroadcastIO())
      val recover = Input(Bool())
    })

    private object State extends ChiselEnum {
      val empty, one, two = Value
    }

    private val stateReg = RegInit(State.empty)
    private val dataReg = Reg(new ExecuteEntry())
    private val shadowReg = Reg(new ExecuteEntry())
    private val deqFire = io.deq.fire || io.sideDeq.fire

    io.enq.ready := (stateReg === State.empty || stateReg === State.one)
    io.sideDeq.valid := (stateReg === State.one || stateReg === State.two) &&
      ((!dataReg.rs1.pending && // Operands are ready
        !dataReg.rs2.pending) || // Or invalid
        !dataReg.valid)

    io.deq.valid := (stateReg === State.one || stateReg === State.two) && !io.sideDeq.valid

    io.deq.bits := dataReg
    io.sideDeq.bits := dataReg

    // Broadcast logic
    for (i <- 0 until 2) { // Bypass
      CoreUtils.matchBroadcast(io.deq.bits.rs1, dataReg.rs1, io.broadcast.entries(i))
      CoreUtils.matchBroadcast(io.deq.bits.rs2, dataReg.rs2, io.broadcast.entries(i))
    }
    for (i <- 0 until 2) {
      // rs1 and rs2
      CoreUtils.matchBroadcast(dataReg.rs1, dataReg.rs1, io.broadcast.entries(i))
      CoreUtils.matchBroadcast(dataReg.rs2, dataReg.rs2, io.broadcast.entries(i))
      CoreUtils.matchBroadcast(shadowReg.rs1, shadowReg.rs1, io.broadcast.entries(i))
      CoreUtils.matchBroadcast(shadowReg.rs2, shadowReg.rs2, io.broadcast.entries(i))
    }

    // Double buffer write logic
    switch(stateReg) {
      is(State.empty) {
        when(io.enq.valid) {
          stateReg := State.one
          dataReg := io.enq.bits
        }
      }

      is(State.one) {
        when(deqFire && !io.enq.valid) {
          stateReg := State.empty
        }
        when(deqFire && io.enq.valid) {
          stateReg := State.one
          dataReg := io.enq.bits
        }
        when(!deqFire && io.enq.valid) {
          stateReg := State.two
          shadowReg := io.enq.bits
        }
      }

      is(State.two) {
        when(deqFire) {
          dataReg := shadowReg
          stateReg := State.one
        }
      }
    }

    // Recovery logic
    when(io.recover) {
      dataReg.valid := false.B
      shadowReg.valid := false.B
    }
  }

  private val buffers = Array.fill(depth)(Module(new DoubleBuffer()))

  for (i <- 0 until depth) {
    buffers(i).io.broadcast := io.broadcast
    buffers(i).io.recover := io.recover
  }

  for (i <- 0 until (depth - 1)) {
    buffers(i + 1).io.enq <> buffers(i).io.deq
  }
  io.enqAndType.enq <> buffers(0).io.enq
  buffers(depth - 1).io.deq.ready := false.B // Block tail

  // Arbitration, from tail to head (old to new)
  // The earlier the instruction is executed, the more instructions can be awakened
  val grant = VecInit.fill(depth)(false.B)
  val notGranted = VecInit.fill(depth)(false.B)
  grant(depth - 1) := buffers(depth - 1).io.sideDeq.valid
  notGranted(depth - 1) := !grant(depth - 1)

  for (i <- (0 to (depth - 2)).reverse) {
    grant(i) := buffers(i).io.sideDeq.valid && notGranted(i + 1)
    notGranted(i) := !grant(i) && notGranted(i + 1)
  }

  buffers.foreach(_.io.sideDeq.ready := false.B)
  io.deq.valid := false.B
  io.deq.bits := new ExecuteEntry().zero
  for (i <- 0 until depth) {
    when(grant(i)) {
      buffers(i).io.sideDeq <> io.deq
    }
  }
}
