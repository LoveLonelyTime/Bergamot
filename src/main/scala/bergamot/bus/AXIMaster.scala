package bergamot.bus

import chisel3._
import chisel3.util._

import bergamot.core.execute.MemoryAccessLength

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/** Core M/MMIO aggregation interface
  *
  * Provide conversion from a set of SMA buses to AXI bus
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

  // Reader logic
  private val readerStatusReg = RegInit(AXIStatus.idle)

  when(readerStatusReg === AXIStatus.idle) {
    when(io.smaReader.valid) {
      readerStatusReg := AXIStatus.address
    }
  }

  when(readerStatusReg === AXIStatus.address) {
    // 32-bit address alignment
    io.axi.ARADDR := io.smaReader.address(31, 2) ## 0.U(2.W)
    io.axi.ARPORT := 0.U // Ignore
    io.axi.ARVALID := true.B
    when(io.axi.ARREADY) {
      readerStatusReg := AXIStatus.response
    }
  }

  when(readerStatusReg === AXIStatus.response) {
    io.axi.RREADY := true.B

    when(io.axi.RVALID) {
      // Byte mapping
      switch(io.smaReader.readType) {
        is(MemoryAccessLength.byte) {
          io.smaReader.data := MuxLookup(io.smaReader.address(1, 0), 0.U)(
            Seq(
              "b00".U -> io.axi.RDATA(7, 0),
              "b01".U -> io.axi.RDATA(15, 8),
              "b10".U -> io.axi.RDATA(23, 16),
              "b11".U -> io.axi.RDATA(31, 24)
            )
          )
        }
        is(MemoryAccessLength.half) {
          io.smaReader.data := Mux(io.smaReader.address(1), io.axi.RDATA(31, 16), io.axi.RDATA(15, 0))
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

  // Writer logic
  private val writerStatusReg = RegInit(AXIStatus.idle)

  when(writerStatusReg === AXIStatus.idle) {
    when(io.smaWriter.valid) {
      writerStatusReg := AXIStatus.address
    }
  }

  when(writerStatusReg === AXIStatus.address) {
    // 32-bit address alignment
    io.axi.AWADDR := io.smaWriter.address(31, 2) ## 0.U(2.W)
    io.axi.AWPORT := 0.U
    io.axi.AWVALID := true.B
    when(io.axi.AWREADY) {
      writerStatusReg := AXIStatus.data
    }
  }

  when(writerStatusReg === AXIStatus.data) {
    // Byte mapping
    switch(io.smaWriter.writeType) {
      is(MemoryAccessLength.byte) {
        io.axi.WDATA := MuxLookup(io.smaWriter.address(1, 0), 0.U)(
          Seq(
            "b00".U -> io.smaWriter.data(7, 0),
            "b01".U -> io.smaWriter.data(7, 0) ## 0.U(8.W),
            "b10".U -> io.smaWriter.data(7, 0) ## 0.U(16.W),
            "b11".U -> io.smaWriter.data(7, 0) ## 0.U(24.W)
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
        io.axi.WDATA := Mux(io.smaWriter.address(1), io.smaWriter.data(15, 0) ## 0.U(16.W), io.smaWriter.data(15, 0))
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

  when(writerStatusReg === AXIStatus.response) {
    io.axi.BREADY := true.B
    when(io.axi.BVALID) {
      io.smaWriter.ready := true.B
      io.smaWriter.error := io.axi.BRESP =/= 0.U
      writerStatusReg := AXIStatus.idle
    }
  }
}
