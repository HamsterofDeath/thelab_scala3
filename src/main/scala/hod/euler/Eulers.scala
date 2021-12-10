package hod.euler
package hod.euler

package hod.euler

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path}
import java.util.stream.Collectors

  @main def euler55 = {
    def isLychrel(bi: BigInt) = {
      def next(bi: BigInt) = bi + BigInt(bi.toString.reverse)
      val matches = Iterator
        .iterate(bi)(next)
        .slice(1, 50)
        .find(e => e.toString == e.toString.reverse)
      matches.isDefined
    }

    val solutions = (1 until 10000).filter(e => isLychrel(e))
    println(9999 - solutions.size)
  }

  @main def euler107 = {

    val originalNetwork = {
      var row = 0
      val data = Array.ofDim[Option[Int]](40, 40)
      Files
        .lines(File("resource/network.txt").toPath)
        .forEach(line => {
          line.split(',').zipWithIndex.foreach {
            case (col, i) =>
              data(i)(row) = if (col=="-") None else Some(col.toInt)
          }
          row += 1
        })
      data.map(_.toVector).toVector
    }
    println(originalNetwork)
}