package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * A basic machine timer
 *
 * CLINT specification
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

object MachineTimer {
  val MTIMECMPL = "h4000".U
  val MTIMECMPH = "h4004".U
  val MTIMEL = "hbff8".U
  val MTIMEH = "hbffc".U
}

/** Machine timer
  *
  * @param base
  *   Base address
  */
class MachineTimer(base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
    val irq = Output(Bool())
    val mtime = Output(UInt(64.W))
  })

  private val mtimeReg = RegInit(0.U(64.W))
  mtimeReg := mtimeReg + 1.U
  io.mtime := mtimeReg

  private val mtimecmpReg = RegInit(0.U(64.W))
  io.irq := mtimeReg >= mtimecmpReg

  io.axi <> new AXIMasterIO().zero

  // AXI logic
  private val readAddressReg = RegInit(0.U(32.W))

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    readAddressReg := io.axi.ARADDR - base.U
  }

  io.axi.RVALID := true.B
  io.axi.RRESP := 0.U

  switch(readAddressReg) {
    is(MachineTimer.MTIMECMPL) { // mtimecmpl
      io.axi.RDATA := mtimecmpReg(31, 0)
    }
    is(MachineTimer.MTIMECMPH) { // mtimecmph
      io.axi.RDATA := mtimecmpReg(63, 32)
    }
    is(MachineTimer.MTIMEL) { // mtimel
      io.axi.RDATA := mtimeReg(31, 0)
    }
    is(MachineTimer.MTIMEH) { // mtimeh
      io.axi.RDATA := mtimeReg(63, 32)
    }
  }

  private val writeAddressReg = RegInit(0.U(32.W))

  when(io.axi.AWVALID) {
    io.axi.AWREADY := true.B
    writeAddressReg := io.axi.AWADDR - base.U
  }

  when(io.axi.WVALID) {
    io.axi.WREADY := true.B
    val data = io.axi.WDATA
    // printf("Current time: %d\n", mtimeReg)
    switch(writeAddressReg) {
      is(MachineTimer.MTIMECMPL) { // mtimecmpl
        mtimecmpReg := mtimecmpReg(63, 32) ## data
        // printf("MTIMER: set mtimecmp = %d\n", mtimecmpReg(63, 32) ## data)
      }
      is(MachineTimer.MTIMECMPH) { // mtimecmph
        mtimecmpReg := data ## mtimecmpReg(31, 0)
        // printf("MTIMER: set mtimecmp = %d\n", data ## mtimecmpReg(31, 0))
      }
      is(MachineTimer.MTIMEL) { // mtimel
        mtimeReg := mtimeReg(63, 32) ## data
        // printf("MTIMER: set mtime = %d\n", mtimeReg(63, 32) ## data)
      }
      is(MachineTimer.MTIMEH) { // mtimeh
        mtimeReg := data ## mtimeReg(31, 0)
        // printf("MTIMER: set mtime = %d\n", data ## mtimeReg(31, 0))
      }
    }
  }

  io.axi.BVALID := true.B
  io.axi.BRESP := 0.U
}
