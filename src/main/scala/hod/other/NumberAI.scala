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

  class Clusters(data: List[PointInAiSpace]) {
    val limit  = 30
    val wisdom = data.groupBy(_.label)

    val condensedMatches = {
      wisdom.view.mapValues(samples => {
        val counts = Array.fill[Int](1024)(0)
        samples.map(_.bitSet).foreach { bs =>
          bs.foreach { i =>
            counts(i) += 1
          }
        }
        BitSet.fromSpecific(counts.zipWithIndex.filter(_._1 > limit).map(_._2))
      }).toMap
    }

    def formattedOnes(label: String) = {
      (0 until 1024).map { i =>
        if (condensedMatches(label)(i)) "1" else "0"
      }
                    .mkString
                    .grouped(32)
                    .mkString("\n")
    }

    def findNearest(data32x32: String): (String, Int) = {
      require(data32x32.length == 1024, s"Length was ${data32x32.length}")
      wisdom.iterator.map({ case (label, dataPoints) => {
        label -> dataPoints.map(_.distanceTo(data32x32)).min
      }
      }).minBy(_._2)
    }
  }

  def main(args: Array[String]): Unit = {
    val data = {
      Random.shuffle(Files.readString(File("resource/bitmap.txt").toPath)
                          .linesIterator
                          .map { block => block.grouped(32).mkString("\n") }
                          .zipWithIndex
                          .map((e, i) => PointInAiSpace(e, i match
                            case n if n < 50 => "0"
                            case n if n < 100 => "1"
                            case _ => "8"
                          ))
                          .toList)
    }

    val c = Clusters(data)
    println(c.formattedOnes("0"))
    println(c.formattedOnes("1"))
    println(c.formattedOnes("8"))
    val result = {
      c.findNearest {
        val testIn =
          s"""00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000
             |00000000000111111111100000000000""".stripMargin
        testIn
          .replace("\r", "")
          .replace("\n", "")
          .trim
      }
    }
    println(result)
  }
}
