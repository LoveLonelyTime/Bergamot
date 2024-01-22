package lltriscv.core.fetch

import chisel3._
import chisel3.util._
import lltriscv.core.DataType
import lltriscv.core.decode.DecodeStageEntry
import lltriscv.utils.ChiselUtils._
import lltriscv.core.record.TLBRequestIO
import lltriscv.cache.ICacheLineRequestIO
import lltriscv.utils.CoreUtils
import lltriscv.core.execute.MemoryErrorCode

/*
 * Instruction fetch
 *
 * Instruction fetch is located behind the instruction cache and is mainly used to control PC logic, instruction validation, and branch prediction
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

class Fetch(cacheLineDepth: Int, queueDepth: Int) extends Module {
  val io = IO(new Bundle {
    val itlb = new TLBRequestIO()
    val icache = new ICacheLineRequestIO(cacheLineDepth)
    val out = DecoupledIO(Vec(2, new DecodeStageEntry()))
    // Prediction failure and correction PC
    val correctPC = Input(DataType.address)
    // Recovery interface
    val recover = Input(Bool())
  })

  private val instructionFetcher = Module(new InstructionFetcher(cacheLineDepth))
  private val speculationStage = Module(new SpeculationStage())
  private val instructionQueue = Module(new InstructionQueue(queueDepth))

  speculationStage.io.in := instructionFetcher.io.out
  instructionFetcher.io.pc := speculationStage.io.pc

  instructionFetcher.io.itlb <> io.itlb
  instructionFetcher.io.icache <> io.icache

  instructionQueue.io.enq <> speculationStage.io.out
  instructionQueue.io.deq <> io.out

  speculationStage.io.correctPC := io.correctPC
  speculationStage.io.recover := io.recover
  instructionQueue.io.recover := io.recover
  instructionFetcher.io.recover := io.recover
}

/** Instruction fetcher
  *
  * Caching two lines at once can perform cross line instructions concatenation.
  *
  * Composed of two independent working state machines:ITLB state machine, ICache state machine.
  *
  * ITLB state machine is responsible for caching the PTE corresponding to the current CacheLine and the PTE corresponding to the next CacheLine.
  *
  * ICache state machine is responsible for caching the current and next CacheLine.
  *
  * @param cacheLineDepth
  *   cache line depth
  */
class InstructionFetcher(cacheLineDepth: Int) extends Module {
  require(cacheLineDepth % 2 == 0, "Instruction Cache line depth must be a multiple of 2")
  require(cacheLineDepth >= 4, "Instruction Cache line depth must be greater than or equal 4")

  val io = IO(new Bundle {
    val out = Output(Vec(2, new RawInstructionEntry()))
    val pc = Input(DataType.address)
    val itlb = new TLBRequestIO()
    val icache = new ICacheLineRequestIO(cacheLineDepth)
    val recover = Input(Bool())
  })

  // Buffer
  private val itlbWorkReg = RegInit(Vec(2, new ITLBWorkEntry()).zero)
  private val lineWorkReg = RegInit(Vec(2, new ICacheLineWorkEntry()).zero)

  // CacheLineAddress |       CacheLineOffset     | byte offset
  //    Remainder     | log2Ceil(cacheLineDepth)  |     1
  private def getCacheLineAddress(address: UInt) = address(31, log2Ceil(cacheLineDepth) + 1) ## 0.U((log2Ceil(cacheLineDepth) + 1).W)
  private def getNextCacheLineAddress(address: UInt) = (address(31, log2Ceil(cacheLineDepth) + 1) + 1.U) ## 0.U((log2Ceil(cacheLineDepth) + 1).W)
  private def getCacheLineOffset(address: UInt) = address(log2Ceil(cacheLineDepth), 1)

  private val lineMatch = VecInit.fill(2)(false.B)
  for (i <- 0 until 2) lineMatch(i) := lineWorkReg(i).valid && lineWorkReg(i).pc === getCacheLineAddress(io.pc)
  private val nextLineMatch = VecInit.fill(2)(false.B)
  for (i <- 0 until 2) nextLineMatch(i) := lineWorkReg(i).valid && lineWorkReg(i).pc === getNextCacheLineAddress(io.pc)

  private def isInBoundary(address: UInt) = getCacheLineOffset(address) === (cacheLineDepth - 1).U

  // TLB logic
  private val itlbMatch = VecInit.fill(2)(false.B)
  for (i <- 0 until 2) itlbMatch(i) := itlbWorkReg(i).valid && itlbWorkReg(i).vpn === getCacheLineAddress(io.pc)(31, 12)
  private val nextITLBMatch = VecInit.fill(2)(false.B)
  for (i <- 0 until 2) nextITLBMatch(i) := itlbWorkReg(i).valid && itlbWorkReg(i).vpn === getNextCacheLineAddress(io.pc)(31, 12)

  private object ITLBStatus extends ChiselEnum {
    val idle, req = Value
  }

  private val itlbStatusReg = RegInit(ITLBStatus.idle)
  private val itlbQueryAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val itlbVictim = RegInit(0.U)
  private val itlbTaskFlag = RegInit(false.B)

  when(itlbStatusReg === ITLBStatus.idle) {
    when(!itlbMatch.reduceTree(_ || _)) { // ITLB missing
      itlbQueryAddressReg := getCacheLineAddress(io.pc)
      itlbVictim := 0.U
      itlbTaskFlag := true.B
      itlbStatusReg := ITLBStatus.req
    }.elsewhen(!nextITLBMatch.reduceTree(_ || _)) { // Next ITLB missing
      itlbQueryAddressReg := getNextCacheLineAddress(io.pc)
      itlbVictim := itlbMatch(0) // Victim is another one
      itlbTaskFlag := true.B
      itlbStatusReg := ITLBStatus.req
    }
  }

  io.itlb.valid := false.B
  io.itlb.vaddress := 0.U
  io.itlb.write := false.B
  when(itlbStatusReg === ITLBStatus.req) {
    io.itlb.vaddress := itlbQueryAddressReg
    io.itlb.valid := true.B
    when(io.itlb.ready) {
      when(itlbTaskFlag) {
        // Not consider 4MiB pages
        itlbWorkReg(itlbVictim).vpn := itlbQueryAddressReg(31, 12)
        itlbWorkReg(itlbVictim).ppn := io.itlb.paddress(31, 12)
        itlbWorkReg(itlbVictim).error := io.itlb.error
        itlbWorkReg(itlbVictim).valid := true.B
      }

      itlbStatusReg := ITLBStatus.idle // Return
    }
  }

  // Cache logic
  private object ICacheStatus extends ChiselEnum {
    val idle, req = Value
  }
  private val iCacheStatusReg = RegInit(ICacheStatus.idle)
  private val iCacheQueryVAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val iCacheQueryPAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val iCacheVictim = RegInit(0.U)
  private val iCacheTaskFlag = RegInit(false.B)

  when(iCacheStatusReg === ICacheStatus.idle) {
    when(!lineMatch.reduceTree(_ || _)) { // Cache line missing
      for (i <- 0 until 2) {
        when(itlbMatch(i)) { // ITLB exists
          when(itlbWorkReg(i).error === MemoryErrorCode.none) { // OK
            iCacheQueryVAddressReg := getCacheLineAddress(io.pc)
            iCacheQueryPAddressReg := itlbWorkReg(i).ppn(19, 0) ## getCacheLineAddress(io.pc)(11, 0)
            iCacheVictim := 0.U
            iCacheTaskFlag := true.B
            iCacheStatusReg := ICacheStatus.req
          }.otherwise { // Error
            lineWorkReg(0).pc := getCacheLineAddress(io.pc)
            lineWorkReg(0).error := itlbWorkReg(i).error
            lineWorkReg(0).valid := true.B
          }
        }
      }
    }.elsewhen(!nextLineMatch.reduceTree(_ || _)) { // Next ITLB missing
      val nextVictim = lineMatch(0) // Victim is another one
      for (i <- 0 until 2) {
        when(nextITLBMatch(i)) { // Next ITLB exists
          when(itlbWorkReg(i).error === MemoryErrorCode.none) { // OK
            iCacheQueryVAddressReg := getNextCacheLineAddress(io.pc)
            iCacheQueryPAddressReg := itlbWorkReg(i).ppn(19, 0) ## getNextCacheLineAddress(io.pc)(11, 0)
            iCacheVictim := nextVictim
            iCacheTaskFlag := true.B
            iCacheStatusReg := ICacheStatus.req
          }.otherwise {
            lineWorkReg(nextVictim).pc := getNextCacheLineAddress(io.pc)
            lineWorkReg(nextVictim).error := itlbWorkReg(i).error
            lineWorkReg(nextVictim).valid := true.B
          }
        }
      }
    }
  }

  io.icache.valid := false.B
  io.icache.address := 0.U
  when(iCacheStatusReg === ICacheStatus.req) {
    io.icache.address := iCacheQueryPAddressReg
    io.icache.valid := true.B

    when(io.icache.ready) {
      when(iCacheTaskFlag) {
        lineWorkReg(iCacheVictim).pc := iCacheQueryVAddressReg
        lineWorkReg(iCacheVictim).content := io.icache.data
        lineWorkReg(iCacheVictim).error := Mux(io.icache.error, MemoryErrorCode.memoryFault, MemoryErrorCode.none)
        lineWorkReg(iCacheVictim).valid := true.B
      }

      iCacheStatusReg := ICacheStatus.idle // Return
    }
  }

  io.out.foreach(_ := new RawInstructionEntry().zero)

  // Merge logic
  for (i <- 0 until 2) {
    when(lineMatch(i)) { // i match PC
      val pcValues = Wire(Vec(2, DataType.address))
      // Get 0 pc
      pcValues(0) := io.pc

      // Get offset
      val offset = Wire(Vec(2, UInt(cacheLineDepth.W)))
      for (i <- 0 until 2)
        offset(i) := getCacheLineOffset(pcValues(i))

      val compress = VecInit(false.B, false.B)
      // Get 0 compress
      compress(0) := lineWorkReg(i).content(offset(0))(1, 0) =/= "b11".U
      // Get 1 pc
      pcValues(1) := Mux(compress(0), pcValues(0) + 2.U, pcValues(0) + 4.U)

      // Get next i
      val nextI = Mux(getCacheLineAddress(pcValues(0)) === getCacheLineAddress(pcValues(1)), i.U, (1 - i).U)

      // Get 0 compress
      compress(1) := lineWorkReg(nextI).content(offset(1))(1, 0) =/= "b11".U

      // Output
      when(lineWorkReg(i).error =/= MemoryErrorCode.none) { // Error
        io.out(0).error := lineWorkReg(i).error
        io.out(0).valid := true.B
      }.elsewhen(compress(0)) { // 16-bits OK
        io.out(0).instruction := lineWorkReg(i).content(offset(0))
        io.out(0).compress := true.B
        io.out(0).valid := true.B
      }.elsewhen(!isInBoundary(pcValues(0))) { // 32-bits OK
        io.out(0).instruction := lineWorkReg(i).content(offset(0) + 1.U) ## lineWorkReg(i).content(offset(0))
        io.out(0).compress := false.B
        io.out(0).valid := true.B
      }.elsewhen(nextLineMatch(1 - i)) { // 32-bits, crossing
        when(lineWorkReg(1 - i).error =/= MemoryErrorCode.none) { // Error
          io.out(0).error := lineWorkReg(1 - i).error
          io.out(0).valid := true.B
        }.otherwise { // OK
          io.out(0).instruction := lineWorkReg(1 - i).content(0) ## lineWorkReg(i).content(offset(0))
          io.out(0).compress := false.B
          io.out(0).valid := true.B
        }
      }

      when(lineWorkReg(i).error =/= MemoryErrorCode.none || lineWorkReg(nextI).pc =/= getCacheLineAddress(pcValues(1))) { // Skip
        io.out(1).valid := false.B
      }.elsewhen(lineWorkReg(nextI).error =/= MemoryErrorCode.none) { // Error
        io.out(1).error := lineWorkReg(nextI).error
        io.out(1).valid := true.B
      }.elsewhen(compress(1)) { // 16-bits OK
        io.out(1).instruction := lineWorkReg(nextI).content(offset(1))
        io.out(1).compress := true.B
        io.out(1).valid := true.B
      }.elsewhen(!isInBoundary(pcValues(1))) { // 32-bits OK
        io.out(1).instruction := lineWorkReg(nextI).content(offset(1) + 1.U) ## lineWorkReg(nextI).content(offset(1))
        io.out(1).compress := false.B
        io.out(1).valid := true.B
      }.elsewhen(nextLineMatch(1 - i)) { // 32-bits, crossing
        when(lineWorkReg(1 - i).error =/= MemoryErrorCode.none) { // Error
          io.out(1).error := lineWorkReg(1 - i).error
          io.out(1).valid := true.B
        }.otherwise { // OK
          io.out(1).instruction := lineWorkReg(1 - i).content(0) ## lineWorkReg(i).content(offset(1))
          io.out(1).compress := false.B
          io.out(1).valid := true.B
        }
      }
    }
  }

  // Recover logic
  when(io.recover) {
    // Clear buffers
    itlbWorkReg.foreach(_.valid := false.B)
    lineWorkReg.foreach(_.valid := false.B)
    // Undo cache tasks
    itlbTaskFlag := false.B
    iCacheTaskFlag := false.B
  }
}

/** Instruction extender
  *
  * Expanding 16 bits instructions to 32 bits
  *
  * C Extensions
  */
class InstructionExtender extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, new RawInstructionEntry()))
    val out = Output(Vec(2, new RawInstructionEntry()))
  })

  def extend(instructionIn: UInt) = {

    val instructionOut = WireInit(instructionIn)
    val imm = WireInit(0.U(32.W))
    val eimm = WireInit(0.U(32.W))
    val rs1 = WireInit(0.U(5.W))
    val rs2 = WireInit(0.U(5.W))
    val rd = WireInit(0.U(5.W))

    switch(instructionIn(1, 0)) {
      is("b01".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.nop, c.addi
            imm := instructionIn(12) ## instructionIn(6, 2)
            eimm := CoreUtils.signExtended(imm, 5)
            instructionOut := eimm(11, 0) ## instructionIn(11, 7) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
          }
          is("b001".U) { // c.jal
            imm := instructionIn(12) ## instructionIn(8) ## instructionIn(10, 9) ## instructionIn(6) ## instructionIn(7) ## instructionIn(2) ## instructionIn(11) ## instructionIn(4, 2) ## 0.U
            eimm := CoreUtils.signExtended(imm, 11)
            instructionOut := eimm(20) ## eimm(10, 1) ## eimm(11) ## eimm(19, 12) ## "b00001".U(5.W) ## "b1101111".U(7.W)
          }
          is("b010".U) { // c.li
            imm := instructionIn(12) ## instructionIn(6, 2)
            eimm := CoreUtils.signExtended(imm, 5)
            instructionOut := eimm(11, 0) ## "b00000".U(5.W) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
          }
          is("b011".U) { // c.addi16sp, c.lui
            when(instructionIn(11, 7) === "b00010".U) { // c.addi16sp
              imm := instructionIn(12) ## instructionIn(4, 3) ## instructionIn(5) ## instructionIn(2) ## instructionIn(6) ## "b0000".U(4.W)
              eimm := CoreUtils.signExtended(imm, 9)
              instructionOut := eimm(11, 0) ## instructionIn(11, 7) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
            }.otherwise { // c.lui
              imm := instructionIn(12) ## instructionIn(6, 2) ## "b000000000000".U(12.W)
              eimm := CoreUtils.signExtended(imm, 17)
              instructionOut := eimm(31, 12) ## instructionIn(11, 7) ## "b0110111".U(7.W)
            }
          }

          is("b100".U) { // c.srli, c.srai, c.andi, c.sub, c.xor, c.or, c.and
            imm := instructionIn(12) ## instructionIn(6, 2)
            eimm := CoreUtils.signExtended(imm, 5)
            rs1 := "b01".U ## instructionIn(9, 7)
            rs2 := "b01".U ## instructionIn(4, 2)
            switch(instructionIn(11, 10)) {
              is("b00".U) { // c.srli
                instructionOut := "b0000000".U(7.W) ## imm(4, 0) ## rs1 ## "b101".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b01".U) { // c.srai
                instructionOut := "b0100000".U(7.W) ## imm(4, 0) ## rs1 ## "b101".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b10".U) { // c.andi
                instructionOut := eimm(11, 0) ## rs1 ## "b111".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b11".U) { // c.sub, c.xor, c.or, c.and
                switch(instructionIn(6, 5)) {
                  is("b00".U) { // c.sub
                    instructionOut := "b0100000".U(7.W) ## rs2 ## rs1 ## "b000".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b01".U) { // c.xor
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b100".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b10".U) { // c.or
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b110".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b11".U) { // c.and
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b111".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                }
              }
            }
          }

          is("b101".U) { // c.j
            imm := instructionIn(12) ## instructionIn(8) ## instructionIn(10, 9) ## instructionIn(6) ## instructionIn(7) ## instructionIn(2) ## instructionIn(11) ## instructionIn(4, 2) ## 0.U
            eimm := CoreUtils.signExtended(imm, 11)
            instructionOut := eimm(20) ## eimm(10, 1) ## eimm(11) ## eimm(19, 12) ## "b00000".U(5.W) ## "b1101111".U(7.W)
          }

          is("b110".U) { // c.beqz
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(12) ## instructionIn(6, 5) ## instructionIn(2) ## instructionIn(11, 10) ## instructionIn(4, 3) ## 0.U
            eimm := CoreUtils.signExtended(imm, 8)
            instructionOut := eimm(12) ## eimm(10, 5) ## "b00000".U(5.W) ## rs1 ## "b000".U(3.W) ## eimm(4, 1) ## eimm(11) ## "b1100011".U(7.W)
          }

          is("b111".U) { // c.bnez
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(12) ## instructionIn(6, 5) ## instructionIn(2) ## instructionIn(11, 10) ## instructionIn(4, 3) ## 0.U
            eimm := CoreUtils.signExtended(imm, 8)
            instructionOut := eimm(12) ## eimm(10, 5) ## "b00000".U(5.W) ## rs1 ## "b001".U(3.W) ## eimm(4, 1) ## eimm(11) ## "b1100011".U(7.W)
          }
        }
      }
      is("b10".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.slli
            rs1 := instructionIn(11, 7)
            imm := instructionIn(12) ## instructionIn(6, 2)
            instructionOut := "b0000000".U(7.W) ## imm(4, 0) ## rs1 ## "b001".U(3.W) ## rs1 ## "b0010011".U(7.W)
          }
          is("b010".U) { // c.lwsp
            rd := instructionIn(11, 7)
            imm := instructionIn(3, 2) ## instructionIn(12) ## instructionIn(6, 4) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## "b00010".U(5.W) ## "b010".U(3.W) ## rd ## "b0000011".U(7.W)
          }
          is("b100".U) { // c.jr, c.mv, c.ebreak, c.jalr, c.add
            when(instructionIn(12)) { // c.ebreak, c.jalr, c.add
              rs1 := instructionIn(11, 7)
              rs2 := instructionIn(6, 2)
              when(rs1 === 0.U && rs2 === 0.U) { // c.ebreak
                instructionOut := "b00000000000100000000000001110011".U(32.W)
              }.elsewhen(rs2 === 0.U) { // c.jalr
                instructionOut := "b000000000000".U(12.W) ## rs1 ## "b000".U(3.W) ## "b00001".U(5.W) ## "b1100111".U(7.W)
              }.otherwise { // c.add
                instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b000".U(3.W) ## rs1 ## "b0110011".U(7.W)
              }
            }.otherwise { // c.jr, c.mv
              when(instructionIn(6, 2) === 0.U) { // c.jr
                rs1 := instructionIn(11, 7)
                instructionOut := "b000000000000".U(12.W) ## rs1 ## "b000".U(3.W) ## "b00000".U(5.W) ## "b1100111".U(7.W)
              }.otherwise { // c.mv
                rd := instructionIn(11, 7)
                rs2 := instructionIn(6, 2)
                instructionOut := "b0000000".U(7.W) ## rs2 ## "b00000".U(5.W) ## "b000".U(3.W) ## rd ## "b0110011".U(7.W)
              }
            }
          }

          is("b110".U) { // c.swsp
            imm := instructionIn(8, 7) ## instructionIn(12, 9) ## "b00".U(2.W)
            rs2 := instructionIn(6, 2)
            instructionOut := imm(11, 5) ## rs2 ## "b00010".U(5.W) ## "b010".U(3.W) ## imm(4, 0) ## "b0100011".U(7.W)
          }
        }
      }
      is("b00".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.addi4spn
            rs1 := "b01".U ## instructionIn(4, 2)
            imm := instructionIn(10, 7) ## instructionIn(12, 11) ## instructionIn(5) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## "b00010".U(5.W) ## "b000".U(3.W) ## rs1 ## "b0010011".U(7.W)
          }
          is("b010".U) { // c.lw
            rd := "b01".U ## instructionIn(4, 2)
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(5) ## instructionIn(12, 10) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## rs1 ## "b010".U(3.W) ## rd ## "b0000011".U(7.W)
          }
          is("b110".U) { // c.sw
            rs1 := "b01".U ## instructionIn(9, 7)
            rs2 := "b01".U ## instructionIn(4, 2)
            imm := instructionIn(5) ## instructionIn(12, 10) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 5) ## rs2 ## rs1 ## "b010".U(3.W) ## imm(4, 0) ## "b0100011".U(7.W)
          }
        }
      }
    }
    instructionOut // Return
  }

  for (i <- 0 until 2) {
    io.out(i).error := io.in(i).error
    io.out(i).compress := io.in(i).compress
    io.out(i).instruction := Mux(io.in(i).compress, extend(io.in(i).instruction), io.in(i).instruction)
    io.out(i).valid := io.in(i).valid
  }
}

/** Instruction predictor
  */
class InstructionPredictor extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(2, new RawInstructionEntry()))
    val pc = Input(DataType.address)
    val out = Output(Vec(2, new SpeculativeEntry()))
    val nextPC = Output(DataType.address)
  })
  val pcValues = Wire(Vec(2, DataType.address))
  pcValues(0) := io.pc
  pcValues(1) := Mux(io.in(0).compress, io.pc + 2.U, io.pc + 4.U)

  val nextPCValues = Wire(Vec(2, DataType.address))
  nextPCValues(0) := pcValues(1)
  nextPCValues(1) := Mux(io.in(1).compress, pcValues(1) + 2.U, pcValues(1) + 4.U)

  // Trivial sepc
  val specValues = Wire(Vec(2, DataType.address))
  specValues(0) := Mux(io.in(0).compress, io.pc + 2.U, io.pc + 4.U)
  specValues(1) := Mux(io.in(1).compress, specValues(0) + 2.U, specValues(0) + 4.U)

  for (i <- 0 until 2) {
    io.out(i).instruction := io.in(i).instruction
    io.out(i).pc := pcValues(i)
    io.out(i).spec := specValues(i)
    io.out(i).next := nextPCValues(i)
    io.out(i).error := io.in(i).error
    io.out(i).valid := io.in(i).valid
  }

  when(io.in(0).valid) {
    when(io.in(1).valid && specValues(0) === pcValues(1)) { // 0 -> 1 -> spec
      io.nextPC := specValues(1)
    }.otherwise { // 0 -> spec, 1 is masked
      io.nextPC := specValues(0)
      io.out(1).valid := false.B
    }
  }.otherwise {
    io.nextPC := io.pc
  }
}

/** Speculation stage
  *
  * Extend instructions and predict
  */
class SpeculationStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Input(Vec(2, new RawInstructionEntry()))
    val out = DecoupledIO(Vec(2, new SpeculativeEntry()))
    // Current PC
    val pc = Output(DataType.address)
    // Prediction failure and correction PC
    val correctPC = Input(DataType.address)
    // Recovery interface
    val recover = Input(Bool())
  })
  // PC register
  private val pcReg = RegInit(DataType.address.zeroAsUInt)

  // Pipeline logic
  private val inReg = RegInit(Vec(2, new RawInstructionEntry()).zero)
  val extender = Module(new InstructionExtender())
  extender.io.in <> inReg

  val predictor = Module(new InstructionPredictor())
  predictor.io.in <> extender.io.out
  predictor.io.pc := pcReg

  when(io.out.fire) { // Sample
    inReg := io.in
    pcReg := predictor.io.nextPC
  }

  // Next PC
  io.pc := predictor.io.nextPC

  // Output
  io.out.valid := true.B
  io.out.bits := predictor.io.out

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
    pcReg := io.correctPC
  }
}

/** Instruction queue
  *
  * @param depth
  *   Instruction queue depth
  */
class InstructionQueue(depth: Int) extends Module {
  require(depth > 0, "Instruction queue depth must be greater than 0")

  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(Vec(2, new SpeculativeEntry())))
    val deq = DecoupledIO(Vec(2, new DecodeStageEntry()))
    val recover = Input(Bool())
  })

  private val queue = RegInit(Vec(depth, Vec(2, new SpeculativeEntry())).zero)

  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = CoreUtils.pointer(depth, incrRead)
  private val (writePtr, nextWrite) = CoreUtils.pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.enq.ready := !fullReg
  io.deq.valid := !emptyReg

  // Queue logic
  private val op = (io.enq.valid && io.enq.ready) ## (io.deq.valid && io.deq.ready)
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

  for (i <- 0 until 2) io.deq.bits <> queue(readPtr)

  // Write logic
  when(doWrite) {
    queue(writePtr) := io.enq.bits
  }

  // Recovery logic
  when(io.recover) {
    queue.foreach(_.foreach(_.valid := false.B))
  }
}
