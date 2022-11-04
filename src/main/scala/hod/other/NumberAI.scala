package hod.other

import java.io.File
import java.net.URL
import java.nio.file.{Files, Paths}
import scala.collection.immutable.BitSet
import scala.util.Random

object NumberAI {

  case class PointInAiSpace(data: String, label: String) {
    val flat           = data.replace("\n", "")
    val bitSet         = {
      BitSet.fromSpecific(flat.zipWithIndex.filter(_._1 == '1').map(_._2))
    }
    val negativeBitSet = {
      BitSet.fromSpecific(flat.zipWithIndex.filter(_._1 == '0').map(_._2))
    }

    require(flat.length == 1024)
    def distanceTo(other: PointInAiSpace): Int = {
      distanceTo(other.flat)
    }

    def distanceTo(other: String): Int = {
      flat.zip(other).map((a, b) => if (a == b) 0 else 1).sum
    }

  }

  class LOOCV(data: List[PointInAiSpace]) {

    def findNearest(data32x32: String) = {
      require(data32x32.length == 1024, s"Length was ${data32x32.length}")
      data
        .iterator
        .filter(_.flat != data32x32)
        .minBy(_.distanceTo(data32x32))
    }

    def loocv(): Unit = {
      val errors = data.filter { testCase =>
        testCase.label != findNearest(testCase.flat).label
      }
      errors.foreach { error =>
        println(s"Incorrectly classified\n${error.data}\nas ${findNearest(error.flat).label}")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val data = {
      Files.readString(File("resource/bitmap.txt").toPath)
                          .linesIterator
                          .map { block => block.grouped(32).mkString("\n") }
                          .zipWithIndex
                          .map((e, i) => PointInAiSpace(e, i match
                            case n if n < 50 => "0"
                            case n if n < 100 => "1"
                            case _ => "8"
                          ))
                          .toList
    }
    val c = LOOCV(data)
    c.loocv()
  }
}
