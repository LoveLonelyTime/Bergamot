package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.utils.ChiselUtils._
import lltriscv.utils.CoreUtils

abstract class BranchPredictor(depth: Int) extends Module {
  require(depth > 0, "Branch predictor table depth must be greater than 0")

  val io = IO(new Bundle {
    val asid = Input(DataType.asid)
    val request = Flipped(new BranchPredictorRequestIO())
    val update = Flipped(new BranchPredictorUpdateIO())
  })
}

class TwoBitsBranchPredictor(depth: Int) extends BranchPredictor(depth) {
  private object History extends ChiselEnum {
    val NN, NT, TN, TT = Value
  }

  private class Entry extends Bundle {
    val asid = DataType.asid
    val pc = DataType.address
    val address = DataType.address
    val history = History()
  }

  private val table = RegInit(Vec(depth, Vec(2, new Entry())).zero)
  private val incrVictim = WireInit(false.B)
  private val (victimPtr, nextVictimPtr) = CoreUtils.pointer(depth, incrVictim)

  // Output
  for (i <- 0 until 2) {
    // Default
    io.request.out(i) := Mux(io.request.in(i).compress, io.request.in(i).pc + 2.U, io.request.in(i).pc + 4.U)
    for (
      j <- 0 until depth;
      k <- 0 until 2
    ) {
      when(io.request.in(i).pc === table(j)(k).pc && io.asid === table(j)(k).asid) {
        // Output logic table
        switch(table(j)(k).history) {
          is(History.NN) { // N
            io.request.out(i) := Mux(io.request.in(i).compress, io.request.in(i).pc + 2.U, io.request.in(i).pc + 4.U)
          }
          is(History.NT) { // T
            io.request.out(i) := table(j)(k).address
          }
          is(History.TN) { // T
            io.request.out(i) := table(j)(k).address
          }
          is(History.TT) { // T
            io.request.out(i) := table(j)(k).address
          }
        }
      }
    }
  }

  // Update logic
  for (i <- 0 until 2) {
    val found = WireInit(false.B)
    when(io.update.entries(i).valid) {
      for (
        j <- 0 until depth;
        k <- 0 until 2
      ) {
        when(io.update.entries(i).pc === table(j)(k).pc && io.asid === table(j)(k).asid) {
          found := true.B
          table(j)(k).address := io.update.entries(i).address
          // Transfer logic
          switch(table(j)(k).history) {
            is(History.NN) {
              table(j)(k).history := Mux(io.update.entries(i).jump, History.NT, History.NN)
            }
            is(History.NT) {
              table(j)(k).history := Mux(io.update.entries(i).jump, History.TT, History.TN)
            }
            is(History.TN) {
              table(j)(k).history := Mux(io.update.entries(i).jump, History.NT, History.NN)
            }
            is(History.TT) {
              table(j)(k).history := Mux(io.update.entries(i).jump, History.TT, History.TN)
            }
          }
        }
      }

      // Replace
      when(!found) {
        table(victimPtr)(i).address := io.update.entries(i).address
        table(victimPtr)(i).pc := io.update.entries(i).pc
        table(victimPtr)(i).asid := io.asid
        table(victimPtr)(i).history := Mux(io.update.entries(i).jump, History.NT, History.NN)
        incrVictim := true.B
      }
    }
  }
}
