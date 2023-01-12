package hod.janestreet

import collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.collection.mutable

import hod.euler.{gcdEuclid, *}

object LessesMore {

  case class Quad(a: Int, b: Int, c: Int, d: Int) {

    def simpler = {
      if (a>0&&b>0&&c>0&&d>0) {
        val div = gcdEuclid(a, gcdEuclid(b, gcdEuclid(c, d))).toInt
        Quad(a / div, b / div, c / div, d / div)
      } else {
        this
      }

    }

    def debug = s"Edges $a,$b,$c,$d, sum $sum"

    def sum = a + b + c + d

    def notAboveLimit = a < 10_000_000 &&
                        b < 10_000_000 &&
                        c < 10_000_000 &&
                        d < 10_000_000
    def next = {
      Quad(math.abs(a - b), math.abs(b - c), math.abs(c - d), math.abs(d - a))
    }

    def generateValues(ab: Int, bc: Int, cd: Int, da: Int): Iterator[(Int, Int, Int, Int)] = {
      val max       = (ab + bc + cd + da + 1)
      val solutions = for {
        a <- (0 to max).iterator if a != ab
        b <- 0 to max if (a - b).abs == ab && b != bc
        c <- 0 to max if (b - c).abs == bc && c != cd
        d <- 0 to max if (c - d).abs == cd && (d - a).abs == da && d != da
      } yield (a, b, c, d)

      solutions
    }

    def prev: Iterator[Quad] = {
      generateValues(a, b, c, d).map((w, x, y, z) => Quad(w, x, y, z))

    }

    def diffAB = math.abs(a - b)
    def diffBC = math.abs(b - c)
    def diffCD = math.abs(c - d)
    def diffDA = math.abs(d - a)
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
    println(steps.size)
  }

  def testRun(q: Quad): Unit = {
    println(primeFactorsOf(q.a).toList)
    println(primeFactorsOf(q.b).toList)
    println(primeFactorsOf(q.c).toList)
    println(primeFactorsOf(q.d).toList)
    println(q.simpler)
    testRun(q.a, q.b, q.c, q.d)
  }

  def countSteps(q: Quad) = {
    //cache.getOrElseUpdate(q, buildSeq(q).size)
    buildSeq(q).size
  }

  val cache = mutable.HashMap.empty[Quad, Int]

  def backwards(q: Quad) = {
    def recur(nq: Quad, step: Int, sum: Int): Unit = {
      val prev = nq.prev
      if (prev.isEmpty) {
        println(s"$nq, $sum, $step")
      } else {
        prev.foreach { nxt =>
          recur(nxt, step + 1, nxt.sum)
        }

      }
    }

    recur(q, 0, q.sum)
  }

  def brandom(): Unit = {
    var best = Quad(0, 0, 0, 0)
    while (true) {
      def random = Math.random() * 10_000_000

      val List(a, b, c, d) = List(random, random,
        random, random).sorted.map(_.toInt)
      val test             = Quad(a, b, c, d)
      if (countSteps(test) > countSteps(best)) {
        println(s"$test, (${test.simpler}) ${countSteps(test)}: ${buildSeq(test).map(_.debug).toList}")
        best = test
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //testRun(Quad(7,999,123,12345678))
    //testRun(Quad(1168608,1737645,2784259,4709289))
    testRun(Quad(2,2,0,2))
    brandom()
    val max      = 800
    println()
    val testUs = for {
      a <- (1 to max).par
      b <- allPrimesLazy.dropWhile(_< a).takeWhile(_ <= max)
      c <- allPrimesLazy.dropWhile(_< b).takeWhile(_ <= max)
      d <- allPrimesLazy.dropWhile(_< c).takeWhile(_ <= max)
    } yield (a, b.toInt, c.toInt, d.toInt)

    println(testUs.size)
    val q      = testUs.iterator.map { case e@(a, b, c, d) =>
      e -> buildSeq(Quad(a, b, c, d)).size
    }.maxBy(_._2)
    println(buildSeq(Quad(q._1._1, q._1._2, q._1._3, q._1._4)).toList)
    println(buildSeq(Quad(q._1._1, q._1._2, q._1._3, q._1._4)).size)
  }
}
