package lltriscv.utils

import chisel3._
import chisel3.util._

object ChiselUtils {

  def int2BinaryString(x: Int, width: Int): String = {
    if (width == 0) {
      ""
    } else {
      (if (((x >> (width - 1)) & 1) == 1) "1" else "0") +
        int2BinaryString(x, width - 1)
    }
  }

  def BinaryString2Int(x: String): Int = {
    def impl(x: String): Int = {
      if (x.length() == 0) {
        0
      } else {
        ((if (x.head == '0') 0 else 1) << (x.length() - 1)) |
          impl(x.tail)
      }
    }
    impl(x.replace("_", ""))
  }

  def int2UInt(x: Int) = {
    s"b${int2BinaryString(x, 32)}".U
  }

  implicit class OperationExtension[T <: Data](x: T) {
    def in(items: T*): Bool = {
      items.foldLeft(false.B)((p, n) => {
        p || x === n
      })
    }

    def in(items: Iterable[T]): Bool = {
      this.in(items.toSeq: _*)
    }
  }
}
