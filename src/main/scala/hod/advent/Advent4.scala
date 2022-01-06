package hod.euler
package hod.advent

import java.beans.BeanInfo
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def data4: List[String] = Utils.loadFile(4, false)

class Bingo(numbers: IndexedSeq[IndexedSeq[Int]]) {
  private val setsOfFive           = {
    val rows = Range(0, 5).map(numbers(_))
    val cols = Range(0, 5).map { row =>
      Range(0, 5).map(col => numbers(col)(row))

    }
    (rows ++ cols).map(mutable.HashSet.empty ++= _)
  }
  private val originalPerSetOfFive = setsOfFive.map(_.toSet)
  private val fed                  = ArrayBuffer.empty[Int]

  def fedCount = fed.size
  def feed(in: Int) = {
    fed += in
    setsOfFive.foreach(_.remove(in))
    this
  }
  def winnerScore: Option[Int] =
    setsOfFive.indexWhere(_.isEmpty) match {
      case -1 => None
      case n =>
        fed.lastOption.map { lastFed =>
          val unmarked = originalPerSetOfFive.flatten.toSet -- fed
          unmarked.sum * lastFed
        }
    }
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
        val vector = grid.map(_.toVector).toVector
        Bingo(vector)
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