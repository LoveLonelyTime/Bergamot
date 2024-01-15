package lltriscv.core.broadcast

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.execute._
import lltriscv.core.record._
import lltriscv.utils.CoreUtils

/*
 * Broadcaster
 *
 * This core adopts a data broadcast/data reception method for data forward.
 * The data required by an instruction is received by waiting for broadcast (aka pending)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Abstract broadcaster
  *
  * A broadcaster selects instructions waiting to be committed from all
  * execution queues through arbitration algorithm. The result of these
  * instructions will be broadcasted through data broadcasting and written back
  * to ROB.
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
abstract class Broadcaster(executeQueueWidth: Int) extends Module {
  require(executeQueueWidth > 0, "Execute queue depth must be greater than 0")
  val io = IO(new Bundle {
    // Execute queue interfaces
    val queues =
      Vec(executeQueueWidth, Flipped(DecoupledIO(new ExecuteResultEntry())))
    // Broadcast interface
    val broadcast = new DataBroadcastIO()
    // ROB table commit interface
    val tableCommit = new ROBTableCommitIO()
  })
}

/** Round-robin broadcaster
  *
  * Adopt round-robin (RR) arbitration algorithm
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class RoundRobinBroadcaster(executeQueueWidth: Int)
    extends Broadcaster(executeQueueWidth) {
  // Single cycle auto increasing loop pointer
  private val (pointer, nextVal) = CoreUtils.pointer(executeQueueWidth, true.B)

  /** Commit queue to entry
    *
    * @param queuePtr
    *   Pointer of queue
    * @param entryPtr
    *   Pointer of entry
    * @return
    */
  def commit(queuePtr: UInt, entryPtr: Int) = {
    when(io.queues(queuePtr).valid) {
      io.queues(queuePtr).ready := true.B
      io.tableCommit.entries(entryPtr) <> io.queues(queuePtr).bits
      when(io.queues(queuePtr).bits.valid) {
        io.broadcast
          .entries(queuePtr)
          .castBroadcast(
            io.queues(queuePtr).bits.rd,
            io.queues(queuePtr).bits.result
          )
      }
    }
  }

  // Commit logic
  io.queues.foreach(_.ready := false.B)
  io.broadcast.entries.foreach(_.noBroadcast())
  io.tableCommit.entries.foreach(_.noResult())

  commit(pointer, 0)
  commit(nextVal, 1)
}
