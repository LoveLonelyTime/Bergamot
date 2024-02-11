package bergamot.bus

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.MemoryAccessLength

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * Core AXI master
 *
 * Core on-chip bus interface master
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Core M/MMIO to on-chip bus interface master
  *
  * Convert from a set of SMA buses to AXI bus by independent read and write FSMs.
  *
  * @note
  *   Misaligned address access not supported
  */
class AXIMaster extends Module {
  val io = IO(new Bundle {
    // In
    val smaReader = Flipped(new SMAReaderIO())
    val smaWriter = Flipped(new SMAWriterIO())
    // Out
    val axi = new AXIMasterIO()
  })

  io.smaReader <> new SMAReaderIO().zero
  io.smaWriter <> new SMAWriterIO().zero
  io.axi <> new AXIMasterIO().zero

  private object AXIStatus extends ChiselEnum {
    val idle, address, data, response = Value;
  }

  // Reader FSM
  private val readerStatusReg = RegInit(AXIStatus.idle)

  switch(readerStatusReg) {
    is(AXIStatus.idle) {
      when(io.smaReader.valid) {
        readerStatusReg := AXIStatus.address
      }
    }

    is(AXIStatus.address) {
      // Align address to word
      io.axi.ARADDR := align(io.smaReader.address, ALIGN_WORD)
      io.axi.ARPORT := 0.U // Ignore port?
      io.axi.ARVALID := true.B

      when(io.axi.ARREADY) {
        readerStatusReg := AXIStatus.response
      }
    }

    is(AXIStatus.response) {
      io.axi.RREADY := true.B

      when(io.axi.RVALID) {
        // Bits trimming (32-bit)
        switch(io.smaReader.readType) {
          is(MemoryAccessLength.byte) {
            val extractByte = extractBits(CoreConstant.byteWidth) _
            io.smaReader.data := MuxLookup(io.smaReader.address(1, 0), 0.U)(
              Seq(
                "b00".U -> extractByte(io.axi.RDATA, 0),
                "b01".U -> extractByte(io.axi.RDATA, 1),
                "b10".U -> extractByte(io.axi.RDATA, 2),
                "b11".U -> extractByte(io.axi.RDATA, 3)
              )
            )
          }
          is(MemoryAccessLength.half) {
            val extractHalf = extractBits(CoreConstant.halfWidth) _
            io.smaReader.data := Mux(io.smaReader.address(1), extractHalf(io.axi.RDATA, 1), extractHalf(io.axi.RDATA, 0))
          }
          is(MemoryAccessLength.word) {
            io.smaReader.data := io.axi.RDATA
          }
        }

        io.smaReader.error := io.axi.RRESP =/= 0.U
        io.smaReader.ready := true.B

        readerStatusReg := AXIStatus.idle
      }
    }
  }

  // Writer FSM
  private val writerStatusReg = RegInit(AXIStatus.idle)

  switch(writerStatusReg) {
    is(AXIStatus.idle) {
      when(io.smaWriter.valid) {
        writerStatusReg := AXIStatus.address
      }
    }

    is(AXIStatus.address) {
      io.axi.AWADDR := align(io.smaWriter.address, ALIGN_WORD)
      io.axi.AWPORT := 0.U
      io.axi.AWVALID := true.B

      when(io.axi.AWREADY) {
        writerStatusReg := AXIStatus.data
      }
    }

    is(AXIStatus.data) {
      switch(io.smaWriter.writeType) {
        is(MemoryAccessLength.byte) {
          val data = extractBits(CoreConstant.byteWidth)(io.smaWriter.data, 0)
          io.axi.WDATA := MuxLookup(io.smaWriter.address(1, 0), 0.U)(
            Seq(
              "b00".U -> data,
              "b01".U -> data ## 0.U(CoreConstant.byteWidth.W),
              "b10".U -> data ## 0.U((CoreConstant.byteWidth * 2).W),
              "b11".U -> data ## 0.U((CoreConstant.byteWidth * 3).W)
            )
          )

          io.axi.WSTRB := MuxLookup(io.smaWriter.address(1, 0), 0.U)(
            Seq(
              "b00".U -> "b0001".U,
              "b01".U -> "b0010".U,
              "b10".U -> "b0100".U,
              "b11".U -> "b1000".U
            )
          )
        }

        is(MemoryAccessLength.half) {
          val data = extractBits(CoreConstant.halfWidth)(io.smaWriter.data, 0)
          io.axi.WDATA := Mux(io.smaWriter.address(1), data ## 0.U(CoreConstant.halfWidth.W), data)
          io.axi.WSTRB := Mux(io.smaWriter.address(1), "b1100".U, "b0011".U)
        }

        is(MemoryAccessLength.word) {
          io.axi.WDATA := io.smaWriter.data
          io.axi.WSTRB := "b1111".U
        }
      }

      io.axi.WVALID := true.B
      when(io.axi.WREADY) {
        writerStatusReg := AXIStatus.response
      }
    }

    is(AXIStatus.response) {
      io.axi.BREADY := true.B
      when(io.axi.BVALID) {
        io.smaWriter.ready := true.B
        io.smaWriter.error := io.axi.BRESP =/= 0.U
        writerStatusReg := AXIStatus.idle
      }
    }
  }
}
