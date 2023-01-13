package hod.janestreet

import collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.collection.mutable

import hod.euler.{gcdEuclid, *}
import collection.parallel.CollectionConverters.IterableIsParallelizable

object LessesMore {

  case class Quad(a: Int, b: Int, c: Int, d: Int) {

    self =>

    def rotated = Quad(b, c, d, a)
    def rotatedReverse = Quad(d, a, b, c)

    def max = a max b max c max d
    def min = a min b min c min d

    lazy val steps: Int = 1 + (if (next.nonZero) next.steps else 0)

    def traverseAll[T](onAnyNode: Quad => T): Unit = {
      var start = self
      val limit  = self.next.max
      while (start.min <= limit || start == self) {
        onAnyNode(start)
        start.generatePrevsV2.foreach(_.traverseAll(onAnyNode))
        start = start.oneMore
      }
    }

    def oneMore = Quad(a + 1, b + 1, c + 1, d + 1)

    def generatePrevsV2 = {
      val ret = mutable.ArrayBuffer.empty[Quad]
      foreachPrevV2(ret += _)
      ret.distinct.toList
    }

    def foreachPrevV2[T](onSolution: Quad => T) = {
      var rotations = 0

      def rotBack(q: Quad) = {
        var count = rotations
        var ret   = q
        while (count > 0) {
          ret = ret.rotatedReverse
          count -= 1
        }
        ret
      }

      var reference = self
      while (rotations < 4) {
        def buildSolutions(valid: mutable.ArrayBuffer[Int]): Unit = {
          def lastAdded = valid.last

          def nextToReach = valid.size match {
            case 1 => reference.a
            case 2 => reference.b
            case 3 => reference.c
            case 4 => reference.d
          }

          if (valid.size < 4) {
            val ifDifferenceRemoved = lastAdded - nextToReach
            if (ifDifferenceRemoved >= 0) {
              valid += ifDifferenceRemoved
              buildSolutions(valid)
              valid.remove(valid.size - 1)
            }
            valid += (lastAdded + nextToReach)
            buildSolutions(valid)
            valid.remove(valid.size - 1)
          } else {
            if (math.abs(valid.last - valid.head) == nextToReach) {
              val mutable.ArrayBuffer(a, b, c, d) = valid
              val quad                            = Quad(a, b, c, d)
              if (quad.nonZero && quad != reference) {
                onSolution(rotBack(quad))
              }
            }
          }
        }

        buildSolutions(mutable.ArrayBuffer(0))

        rotations += 1
        reference = reference.rotated
      }
    }

    def debug = "".padTo(steps, ' ') + s"-> Edges $a,$b,$c,$d, sum $sum, depth $steps"

    def sum = a + b + c + d

    lazy val next = {
      Quad(math.abs(a - b), math.abs(b - c), math.abs(c - d), math.abs(d - a))
    }

    def nonZero = a + b + c + d > 0
  }

  def buildSeq(start: Quad) = {
    Iterator.iterate(start)(_.next).takeWhilePlusOne { e =>
      e.nonZero
    }
  }

  def testRun(a: Int, b: Int, c: Int, d: Int): Unit = {
    val steps = buildSeq(Quad(a, b, c, d)).toList
    println(steps.map(_.debug).mkString("\n"))
    if (a > 0) {
      println(
        steps.reverse.sliding(2, 1).map { case List(a, b) => a.a.toDouble / b.a }.mkString(", "))
      println(steps.reverse.sliding(2, 1).map { case List(a, b) => b.a - a.a }.mkString(", "))
      println(s"GCD ${steps.filter(_.a > 0).map(_.a.toLong).reduce(gcdEuclid)}")
    }
    println(steps.size)
  }

  def testRun(q: Quad): Unit = {
    testRun(q.a, q.b, q.c, q.d)
  }

  def main(args: Array[String]): Unit = {
    var best  = Option.empty[Quad]
    val guess = 1 << 13
    var count = 0

    Quad(guess, guess, guess, guess)
      .traverseAll { candidate =>
        count += 1
        if (best.isEmpty || best.get.steps < candidate.steps ||
            best.get.steps == candidate.steps && best.get.sum > candidate.sum) {
          println(s"NEXT: $count")
          testRun(candidate)
          best = Some(candidate)
        }
      }
    println(best)
    println("!!! " + count + "@ "+guess)
  }
}
