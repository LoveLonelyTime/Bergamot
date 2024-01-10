package lltriscv.test

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import lltriscv.core.fetch.InstructionFetcher

class InstructionFetcherTest extends AnyFreeSpec with ChiselScalatestTester {
  "InstructionFetcher should be OK" in {
    def pipelineOutput(
        dut: InstructionFetcher,
        width: Int,
        instructions: UInt*
    ) = {
      dut.io.fetcher.pc.ready.poke(true.B)
      instructions
        .grouped(width)
        .foreach(group => {
          while (!dut.io.fetcher.pc.valid.peekBoolean()) dut.clock.step()
          for (i <- 0 until group.length) {
            dut.io.fetcher.instruction.bits(i).instruction.poke(group(i))
            dut.io.fetcher.instruction
              .bits(i)
              .pc
              .poke((dut.io.fetcher.pc.bits.peekInt() + i).U)
            dut.io.fetcher.instruction.bits(i).vaild.poke(true.B)
          }
          for (i <- group.length until width) {
            dut.io.fetcher.instruction.bits(i).instruction.poke(0.U)
            dut.io.fetcher.instruction
              .bits(i)
              .pc
              .poke(0.U)
            dut.io.fetcher.instruction.bits(i).vaild.poke(false.B)
          }
          dut.io.fetcher.instruction.valid.poke(true.B)

          while (!dut.io.fetcher.instruction.ready.peekBoolean())
            dut.clock.step()
          dut.clock.step()
        })

      dut.io.fetcher.pc.ready.poke(false.B)
      dut.io.fetcher.instruction.valid.poke(false.B)
      dut.clock.step()
    }

    def delayOutput(
        dut: InstructionFetcher,
        width: Int,
        instructions: UInt*
    ) = {

      instructions
        .grouped(width)
        .foreach(group => {
          dut.io.fetcher.instruction.valid.poke(false.B)
          dut.io.fetcher.pc.ready.poke(true.B)
          while (!dut.io.fetcher.pc.valid.peekBoolean()) dut.clock.step()
          dut.clock.step()
          // wait to read from memory
          dut.io.fetcher.pc.ready.poke(false.B)
          dut.clock.step(10)

          // OK

          for (i <- 0 until group.length) {
            dut.io.fetcher.instruction.bits(i).instruction.poke(group(i))
            dut.io.fetcher.instruction
              .bits(i)
              .pc
              .poke((dut.io.fetcher.pc.bits.peekInt() + i).U)
            dut.io.fetcher.instruction.bits(i).vaild.poke(true.B)
          }
          for (i <- group.length until width) {
            dut.io.fetcher.instruction.bits(i).instruction.poke(0.U)
            dut.io.fetcher.instruction
              .bits(i)
              .pc
              .poke(0.U)
            dut.io.fetcher.instruction.bits(i).vaild.poke(false.B)
          }
          dut.io.fetcher.instruction.valid.poke(true.B)

          while (!dut.io.fetcher.instruction.ready.peekBoolean())
            dut.clock.step()
          dut.clock.step()
        })

      dut.io.fetcher.pc.ready.poke(false.B)
      dut.io.fetcher.instruction.valid.poke(false.B)
      dut.clock.step()
    }
    test(new InstructionFetcher(2, 8))
      .withAnnotations(Seq(chiseltest.simulator.WriteVcdAnnotation)) { dut =>
        delayOutput(dut, 2, 13.U, 14.U, 15.U, 16.U, 17.U)

        dut.io.puller.ready.poke(true.B)
        for (i <- 0 until 50) {
          if (dut.io.puller.valid.peekBoolean()) {
            println("Get once instructions from fifo:")
            dut.io.puller.bits.zipWithIndex.foreach(group => {
              println(
                s"--Index: ${group._2}, Instruction: ${group._1.instruction
                    .peekInt()}, PC: ${group._1.pc
                    .peekInt()}, Vaild: ${group._1.vaild.peekBoolean()}"
              )
            })
          }
          dut.clock.step()
        }
      }
  }
}
