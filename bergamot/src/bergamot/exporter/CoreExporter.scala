package bergamot.exporter

import chisel3._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

import bergamot.core._

object conf {
  val BANNER = """
00000000  00000000 00000000   111111      000    00     00  0000000  11111111 
00     00 00       00     00 11    11    00 00   000   000 00     00    11    
00     00 00       00     00 11         00   00  0000 0000 00     00    11    
00000000  000000   00000000  11   1111 00     00 00 000 00 00     00    11    
00     00 00       00   00   11    11  000000000 00     00 00     00    11    
00     00 00       00    00  11    11  00     00 00     00 00     00    11    
00000000  00000000 00     00  111111   00     00 00     00  0000000     11    
"""

  val THANK = """
----------------------------------------------
|       Thank you for using Bergamot!        |
| https://github.com/LoveLonelyTime/Bergamot |
|               BergamotCore                 |
|                                            |
----------------------------------------------
"""

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      // make yosys happy
      // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _)
    // Enable Chisel printf
    // "-enable-layers=Verification"
  )
}

object CoreExporter extends App {
  println(conf.BANNER)
  circt.stage.ChiselStage.emitSystemVerilogFile(new BergamotCore(CoreConfig.default), args, conf.firtoolOptions)
  println(conf.THANK)
}

object CoreExporterSim extends App {
  println(conf.BANNER)
  circt.stage.ChiselStage.emitSystemVerilogFile(new VerilatorTestCore(), args, conf.firtoolOptions)
  println(conf.THANK)
}
