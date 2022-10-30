package hod.other

import hod.euler
import hod.euler.measured

object CountTo1Billion {
  def main(args: Array[String]): Unit = {
    measured {
      var n = 0
      while (n < 1_000_000_000) n += 1

      println(s"Counted to $n")
    }

  }
}
