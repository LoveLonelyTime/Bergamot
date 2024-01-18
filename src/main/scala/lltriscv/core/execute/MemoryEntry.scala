package lltriscv.core.execute

import chisel3._
import chisel3.util._

import lltriscv.core._

/** Memory operation type
  */
object MemoryOperationType extends ChiselEnum {
  /*
   * Reserve operation:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Load operations:
   * - lb: load byte
   * - lh: load half word
   * - lw: load word
   * - lbu: load unsigned byte
   * - lhu: load unsigned half word
   *
   * Store operations:
   * - sb: store byte
   * - sh: store half word
   * - sw: store word
   *
   */
  val none, undefined, lb, lh, lw, lbu, lhu, sb, sh, sw = Value

  val readValues = List(lb, lh, lw, lbu, lhu)
  val writeValues = List(sb, sh, sw)
}

/** Memory access length
  */
object MemoryAccessLength extends ChiselEnum {
  val word, half, byte = Value
}

/** Memory error code
  */
object MemoryErrorCode extends ChiselEnum {
  /*
   * misaligned: Access address is misaligned
   * pageFault: Page table fault
   * memoryFault: Memory fault
   */
  val none, misaligned, pageFault, memoryFault = Value
}

/** Memory execute stage entry
  *
  * The input entry of MemoryExecuteStage
  */
class MemoryExecuteStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val add1 = DataType.operation // Address addend 1
  val add2 = DataType.operation // Address addend 2
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Memory TLB stage entry
  *
  * The input entry of MemoryTLBStage
  */
class MemoryTLBStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val error = MemoryErrorCode() // Memory error code
  val vaddress = DataType.address // Virtual address
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Memory read write stage entry
  *
  * The input entry of MemoryReadWriteStage
  */
class MemoryReadWriteStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val error = MemoryErrorCode() // Memory error code
  val vaddress = DataType.address // Virtual address
  val paddress = DataType.address // Physical address
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
