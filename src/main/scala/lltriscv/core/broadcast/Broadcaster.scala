package lltriscv.core.broadcast

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.execute._

class Broadcaster(width: Int) extends Module {
  val io = IO(new Bundle {
    val queues = Vec(width, Flipped(DecoupledIO(new ExecuteResultEntry())))
    val broadcast = Flipped(new DataBroadcastIO())
  })
}

class PriorityBroadcaster(width: Int) extends Broadcaster(width) {
  val grant1 = VecInit.fill(width)(false.B)
  val notGrant1 = VecInit.fill(width)(false.B)

  val grant2 = VecInit.fill(width)(false.B)
  val notGrant2 = VecInit.fill(width)(false.B)

  grant1(0) := io.queues(0).valid
  notGrant1(0) := !grant1(0)
  for (i <- 1 until width) {
    grant1(i) := io.queues(i).valid && notGrant1(i - 1)
    notGrant1(i) := !grant1(i) && notGrant1(i - 1)
  }

  grant2(0) := false.B
  notGrant2(0) := true.B
  for (i <- 1 until width) {
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
  for (i <- 0 until width) {
    when(grant1(i)) {
      io.broadcast.entries(0).valid := true.B
      io.broadcast.entries(0).data := io.queues(i).bits.result
      io.broadcast.entries(0).receipt := io.queues(i).bits.rd
      io.queues(i).ready := true.B
    }.elsewhen(grant2(i)) {
      io.broadcast.entries(1).valid := true.B
      io.broadcast.entries(1).data := io.queues(i).bits.result
      io.broadcast.entries(1).receipt := io.queues(i).bits.rd
      io.queues(i).ready := true.B
    }
  }
}
