package lltriscv.core.broadcast

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.execute._
import lltriscv.core.record._

class Broadcaster(executeQueueWidth: Int) extends Module {
  val io = IO(new Bundle {
    val queues =
      Vec(executeQueueWidth, Flipped(DecoupledIO(new ExecuteResultEntry())))
    val broadcast = new DataBroadcastIO()

    val tableCommit = new ROBTableCommitIO()
  })
}

class PriorityBroadcaster(executeQueueWidth: Int)
    extends Broadcaster(executeQueueWidth) {
  val grant1 = VecInit.fill(executeQueueWidth)(false.B)
  val notGrant1 = VecInit.fill(executeQueueWidth)(false.B)

  val grant2 = VecInit.fill(executeQueueWidth)(false.B)
  val notGrant2 = VecInit.fill(executeQueueWidth)(false.B)

  grant1(0) := io.queues(0).valid
  notGrant1(0) := !grant1(0)
  for (i <- 1 until executeQueueWidth) {
    grant1(i) := io.queues(i).valid && notGrant1(i - 1)
    notGrant1(i) := !grant1(i) && notGrant1(i - 1)
  }

  grant2(0) := false.B
  notGrant2(0) := true.B
  for (i <- 1 until executeQueueWidth) {
    when(grant1(i)) { // skip
      grant2(i) := false.B
      notGrant2(i) := notGrant2(i - 1)
    }.otherwise {
      grant2(i) := io.queues(i).valid && notGrant2(i - 1)
      notGrant2(i) := !grant2(i) && notGrant2(i - 1)
    }
  }

  io.broadcast.entries(0).valid := false.B
  io.broadcast.entries(0).data := 0.U
  io.broadcast.entries(0).receipt := 0.U

  io.broadcast.entries(1).valid := false.B
  io.broadcast.entries(1).data := 0.U
  io.broadcast.entries(1).receipt := 0.U

  io.tableCommit.entries(0).id := 0.U
  io.tableCommit.entries(0).result := 0.U
  io.tableCommit.entries(0).valid := false.B
  io.tableCommit.entries(1).id := 0.U
  io.tableCommit.entries(1).result := 0.U
  io.tableCommit.entries(1).valid := false.B
  io.tableCommit.wen := false.B

  io.queues.foreach(_.ready := false.B)

  for (i <- 0 until executeQueueWidth) {
    when(grant1(i)) {
      when(io.queues(i).bits.valid) {
        io.broadcast.entries(0).valid := true.B
        io.broadcast.entries(0).data := io.queues(i).bits.result
        io.broadcast.entries(0).receipt := io.queues(i).bits.rd

        io.tableCommit.entries(0).id := io.queues(i).bits.rd
        io.tableCommit.entries(0).result := io.queues(i).bits.result
        io.tableCommit.entries(0).valid := true.B
        io.tableCommit.wen := true.B
      }

      io.queues(i).ready := true.B

    }.elsewhen(grant2(i)) {
      when(io.queues(i).bits.valid) {
        io.broadcast.entries(1).valid := true.B
        io.broadcast.entries(1).data := io.queues(i).bits.result
        io.broadcast.entries(1).receipt := io.queues(i).bits.rd

        io.tableCommit.entries(1).id := io.queues(i).bits.rd
        io.tableCommit.entries(1).result := io.queues(i).bits.result
        io.tableCommit.entries(1).valid := true.B
        io.tableCommit.wen := true.B
      }
      io.queues(i).ready := true.B

    }
  }
}
