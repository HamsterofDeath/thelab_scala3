package hod.euler
package hod.advent

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def data4: List[String] = Utils.loadFile(4, false)

class Bingo(rows: IndexedSeq[IndexedSeq[Int]]) {
  private val setsOfFive      = {
    val cols = Range(0, 5).map { row =>
      Range(0, 5).map(col => rows(col)(row))

    }
    (rows ++ cols).map(mutable.HashSet.empty ++= _)
  }
  private val originalNumbers = setsOfFive.flatten.toSet
  private val fed             = ArrayBuffer.empty[Int]

  def fedCount = fed.size
  def feed(in: Int) = {
    fed += in
    setsOfFive.foreach(_.remove(in))
    this
  }
  def winnerScore: Option[Int] =
    if (setsOfFive.exists(_.isEmpty)) {
      fed.lastOption.map { lastFed =>
        (originalNumbers -- fed).sum * lastFed
      }
    } else None
}

val (bingoNumberFeed, bingoFields) = {
  val lines   = data4
  val numbers = lines.head.split(',').toList.map(_.trim.toInt)
  val bingos  = {
    lines
      .drop(2)
      .grouped(6)
      .map { block =>
        val grid   = block.map(_.grouped(3).map(_.trim.toInt)).take(5)
        Bingo(grid.map(_.toVector).toVector)
      }.toVector
  }
  (numbers, bingos)
}

@main def advent4Easy: Unit = {
  bingoNumberFeed.foreach { in =>
    bingoFields.foreach(_.feed(in))
    val winner   = bingoFields.filter(_.winnerScore.isDefined)
    val solution = winner.flatMap(_.winnerScore)
    if (solution.nonEmpty) {
      println(solution)
      return;
    }
  }
}

@main def advent4Hard: Unit = {
  val worst = {
    bingoFields.map { field =>
      bingoNumberFeed
        .iterator
        .map(field.feed)
        .takeWhile(_.winnerScore.isEmpty)
        .toList
        .last
    }.maxBy(_.fedCount)
  }
  println(worst.winnerScore)
}