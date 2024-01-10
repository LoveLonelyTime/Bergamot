package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import lltriscv.core._
import lltriscv.core.decode.IssueStage

// class IssueStageTest extends AnyFreeSpec with ChiselScalatestTester {
//   "IssueStageTest should be OK" in {
//     test(new IssueStage()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       def putInstruction(id: Int, queue: ExecuteQueueType.Type) = {
//         dut.io.in(id).instructionType.poke(InstructionType.S)
//         dut.io.in(id).pc.poke(15.U)
//         dut.io.in(id).opcode.poke("b0100011".U)
//         dut.io.in(id).executeQueue.poke(queue)
//         dut.io.in(id).vaild.poke((queue != ExecuteQueueType.none).B)
//       }
//       dut.io.aluEnq.ready.poke(true.B)
//       dut.io.memoryEnq.ready.poke(true.B)
//       dut.io.branchEnq.ready.poke(true.B)

//       putInstruction(0, ExecuteQueueType.alu)
//       putInstruction(1, ExecuteQueueType.branch)
//       while (!dut.io.ready.peekBoolean()) { dut.clock.step() }
//       dut.clock.step()
//       putInstruction(0, ExecuteQueueType.memory)
//       putInstruction(1, ExecuteQueueType.memory)
//       while (!dut.io.ready.peekBoolean()) { dut.clock.step() }
//       dut.clock.step()

//       dut.io.memoryEnq.ready.poke(false.B)
//       putInstruction(0, ExecuteQueueType.memory)
//       putInstruction(1, ExecuteQueueType.alu)
//       dut.clock.step(5)
//       dut.io.memoryEnq.ready.poke(true.B)
//       while (!dut.io.ready.peekBoolean()) { dut.clock.step() }
//       dut.clock.step(5)
//     }
//   }
// }
