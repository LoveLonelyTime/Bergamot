package bergamot.core.debug

import chisel3._

class DebugIO extends Bundle {
  val hit = Output(Bool())
}
