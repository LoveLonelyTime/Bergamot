package lltriscv.core.record

import chisel3._
import chisel3.util._
import lltriscv.utils.CoreUtils

class StoreQueue(depth: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(new StoreQueueAllocIO())
    val deq = DecoupledIO(new StoreQueueDequeueEntry())
    val retire = Flipped(new StoreQueueRetireIO())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Read/Wirte pointers
  private val queue = Reg(Vec(depth, new StoreQueueEntry()))

  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = CoreUtils.pointer(depth, incrRead)
  val (writePtr, nextWrite) = CoreUtils.pointer(depth, incrWrite)

  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)

  io.alloc.ready := !fullReg
  io.alloc.id := writePtr

  // In ordered arbitration logic
  io.deq.valid := !emptyReg && // Not empty
    (
      queue(readPtr).retire || // Retired
        !queue(readPtr).valid // Or invalid
    )

  io.deq.bits.writeType := queue(readPtr).writeType
  io.deq.bits.data := queue(readPtr).data
  io.deq.bits.address := queue(readPtr).address
  io.deq.bits.valid := queue(readPtr).valid

  // Queue logic
  private val op = (io.alloc.valid && io.alloc.ready) ##
    (io.deq.valid && io.deq.ready)
  private val doWrite = WireDefault(false.B)

  switch(op) {
    is("b01".U) { // dequeue
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
    }
    is("b10".U) { // alloc
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
      doWrite := true.B
    }
    is("b11".U) { // alloc-dequeue
      incrRead := true.B
      incrWrite := true.B
      doWrite := true.B
    }
  }

  // Write logic
  when(doWrite) {
    queue(writePtr).writeType := io.alloc.writeType
    queue(writePtr).address := io.alloc.address
    queue(writePtr).data := io.alloc.data
    queue(writePtr).valid := true.B
    queue(writePtr).retire := false.B

    printf("StoreQueue: enq address = %d, data = %d, id = %d\n", io.alloc.address, io.alloc.data, writePtr)
  }

  // Retire logic
  for (i <- 0 until 2) {
    when(io.retire.entries(i).en) {
      queue(io.retire.entries(i).id).retire := true.B
      printf("StoreQueue: Retire id = %d\n", io.retire.entries(i).id)
    }
  }

  // Recovery logic
  when(io.recover) {
    queue.foreach(item => {
      when(!item.retire) { // Skip item retired
        item.valid := false.B
      }
    })
  }
}
