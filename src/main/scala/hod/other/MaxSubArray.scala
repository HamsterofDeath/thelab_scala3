package hod.other

import scala.collection.mutable.ArrayBuffer

object MaxSubArray {
  def main(args: Array[String]): Unit = {
    val test    = Array(100,1, -22, 3, 4, -5, 6,-550,100)
    val current = ArrayBuffer.empty[Int]
    var largest = List.empty[Int]
    var cursor  = 0
    while (cursor < test.length) {
      current += test(cursor)
      if (current.sum<0) {
        current.clear()
      }

      if (current.sum > largest.sum) {
        largest = current.toList
      }
      cursor += 1

    }
    println(largest)
  }
}
