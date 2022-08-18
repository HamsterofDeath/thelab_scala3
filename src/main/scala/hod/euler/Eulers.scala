package hod.euler

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicLong
import java.util.stream.Collectors
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.seqIsParallelizable

@main def euler55(): Unit = {
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

@main def euler107(): Unit = {
  val txt             = "resource/network.txt"
  val size            = 40
  val originalNetwork = {
    var row  = 0
    val data = Array.ofDim[Option[Int]](size, size)
    Files
      .lines(File(txt).toPath)
      .forEach { line =>
        line.split(',').zipWithIndex.foreach {
          case (col, i) =>
            data(i)(row) = if (col == "-") None else Some(col.toInt)
        }
        row += 1
      }
    data.map(_.toVector).toVector
  }

  case class Connection(from: Int, to: Int, cost: Int)

  def optimize(network: IndexedSeq[IndexedSeq[Option[Int]]]) = {
    val stillOpen = mutable.HashSet.empty[Int] ++= 1 until size
    val covered   = mutable.HashSet.empty[Int] += 0

    def findNextShortest = {
      val candidates = covered.map { i =>
        network(i)
          .zipWithIndex
          .filter(_._1.isDefined)
          .filter(e => stillOpen(e._2))
          .map { (value, where) => (value.get, where) }
          .minByOption(_._1)
          .map { shortest =>
            Connection(i, shortest._2, shortest._1)
          }
      }

      candidates.flatten.minBy(_.cost)
    }

    val connections = {
      val optimized = ArrayBuffer.empty[Connection]
      while (stillOpen.nonEmpty) {
        val addThis = findNextShortest

        def padInt(i: Int) = i.toString.reverse.padTo(4, ' ').reverse

        println(s"Adding to node ${padInt(addThis.from)} connection to ${
          padInt(addThis.to)
        }, cost ${padInt(addThis.cost)}")
        stillOpen -= addThis.to
        covered += addThis.to
        optimized += addThis
      }
      optimized.toList
    }
    connections
  }

  val betterNetwork = optimize(originalNetwork)
  val initialCost   = originalNetwork.map(_.map(_.getOrElse(0)).sum).sum / 2
  val optimizedCost = betterNetwork.map(_.cost).sum
  val saved         = initialCost - optimizedCost
  println(initialCost)
  println(optimizedCost)
  println(saved)
}

@main def euler359(): Unit = {
  val fxr     = 71328803586048L
  val limit   = math.sqrt(fxr).toLong
  val factors = {
    Iterator.iterate(1L) {_ + 1}
            .takeWhile(_ <= limit)
            .flatMap { i =>
              if (fxr % i == 0) {
                List((i, fxr / i), (fxr / i, i))
              } else {
                Nil
              }
            }.toList
            .distinct
            .sorted
  }

  factors.foreach(println)
}

@main def euler230(): Unit = {
  val cache = mutable.HashMap.empty[Int, Long]

  val A  = 'A'
  val B  = 'B'
  val ab = List(A, B)

  def stupidSequenceGenerate(n: Int): String = ab(n).toString

  def fib(n: Int): Long = {
    n match {
      case 0 | 1 => 1
      case n => cache.getOrElseUpdate(n, fib(n - 2) + fib(n - 1))
    }
  }

  def fibs = Iterator.from(0).map(fib)

  def indexOfNextLargerFib(n: Long) = {
    fibs.indexWhere(_ > n)
  }

  val lookupLimit = 2
  val lookUp      = List.tabulate(lookupLimit)(stupidSequenceGenerate)

  val maxN = 17

  def nToIndexInSequence(n: Int) = {
    ((127 + 19 * n) * BigInt(7).pow(n)).bigInteger.longValueExact()
  }

  val test        = false
  val a           = if (test) "1415926535" else
    "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
  val b           = if (test) "8979323846" else
    "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
  val elementSize = a.length

  def solveFor(nInInitialSequence: Long) = {
    @tailrec
    def solveForTuple(nInSequence: Long, sequenceIndex: Int): Boolean = {
      if (lookupLimit > sequenceIndex) {
        lookUp(sequenceIndex)(nInSequence.toInt) == A
      } else {
        val leftSubSequenceIndex = sequenceIndex - 2
        val isInLeftPart         = nInSequence < fib(leftSubSequenceIndex)
        if (isInLeftPart) {
          solveForTuple(nInSequence, leftSubSequenceIndex)
        } else {
          val rightSubSequenceIndex = leftSubSequenceIndex + 1
          val newIndex              = nInSequence - fib(leftSubSequenceIndex)
          solveForTuple(newIndex, rightSubSequenceIndex)
        }
      }
    }

    val index = indexOfNextLargerFib(nInInitialSequence)
    solveForTuple(nInInitialSequence, index)
  }

  def dab(n: Long): Int = {
    val indexOnCompressedSequence = n / elementSize
    val remainder                 = (n % elementSize)
    require(remainder.isValidInt)
    val isA       = solveFor(indexOnCompressedSequence)
    val character = if (isA) {
      a(remainder.toInt)
    } else {
      b(remainder.toInt)
    }
    Character.getNumericValue(character)
  }

  val solution = {
    if (test) {
      dab(34)
    } else {
      (0 to maxN).map { n =>
        val index = nToIndexInSequence(n) - 1
        BigInt(10).pow(n) * dab(index)
      }.sum

    }
  }
  println(solution)

}

@main def euler17(): Unit = {
  def spell(n: Int): String = {
    n match {
      case n if n < 10 =>
        n match {
          case 0 => ""
          case 1 => "one"
          case 2 => "two"
          case 3 => "three"
          case 4 => "four"
          case 5 => "five"
          case 6 => "six"
          case 7 => "seven"
          case 8 => "eight"
          case 9 => "nine"
        }
      case n if n >= 10 && n <= 19 =>
        n match {
          case 10 => "ten"
          case 11 => "eleven"
          case 12 => "twelve"
          case 13 => "thirteen"
          case 14 => "fourteen"
          case 15 => "fifteen"
          case 16 => "sixteen"
          case 17 => "seventeen"
          case 18 => "eighteen"
          case 19 => "nineteen"
        }
      case n if n <= 99 =>
        val left   = n / 10 match {
          case 2 => "twenty"
          case 3 => "thirty"
          case 4 => "forty"
          case 5 => "fifty"
          case 6 => "sixty"
          case 7 => "seventy"
          case 8 => "eighty"
          case 9 => "ninety"
        }
        val hyphen = if (n % 10 > 0) "-" else ""
        left + hyphen + spell(n % 10)
      case n if n < 1000 =>
        val and = if (n % 100 > 0) " and " else ""
        spell(n / 100) + " hundred" + and + spell(n % 100)
      case n if n < 10000 =>
        spell(n / 1000) + " thousand " + spell(n % 1000)
    }
  }

  val solution = (1 to 1000).map { i =>
    spell(i).count(_.isLetter)
  }.sum

  println(solution)

}

@main def euler5(): Unit = {
  val test     = 1 to 20
  val solution = Iterator.from(1).find { e =>
    val candidate = e * 20L
    test.forall(candidate % _ == 0)
  }
  println(solution.get * 20)
}

@main def euler73(): Unit = {
  def isReducedProperFraction(n: Int, d: Int) = 1 == gcdEuclid(n, d)

  def gcdEuclid(a: Int, b: Int) = {
    var max       = Math.max(a, b)
    var min       = Math.min(a, b)
    var remainder = max % min
    while (remainder != 0) {
      max = min
      min = remainder
      remainder = max % min
    }
    min
  }

  def isASmallerThanB(n1: Int, d1: Int, n2: Int, d2: Int) = n1 * d2.toLong < n2 * d1.toLong

  val ds       = 1 to 12000
  val solution = ds.map { d =>
    val ns = (d / 3) to (d / 2)
    ns.count { n =>
      isASmallerThanB(1, 3, n, d) &&
      isASmallerThanB(n, d, 1, 2) &&
      isReducedProperFraction(n, d)
    }
  }.sum
  println(solution)
}

@main def euler85(): Unit = {

  def countPlacements(w: Int, h: Int, tw: Int, th: Int) = {
    (((tw - w) + 1) * ((th - h) + 1)).toLong
  }

  val solution = Iterator.from(1).take(100).flatMap { tw =>
    Iterator.from(1).take(tw).map { th =>
      val totalPlacements = {
        1 to tw flatMap { w =>
          1 to th map { h =>
            countPlacements(w, h, tw, th)
          }
        }
      }
      ((tw, th), totalPlacements.sum)
    }
  }
  val best     = solution.toList.minBy(e => (2000000 - e._2).abs)._1
  println(best._1 * best._2)
}

@main def euler225(): Unit = {
  def tribonacciMods(modBy: Int) = {
    var a, b, c = 1

    def seen(a: Int, b: Int, c: Int) = a == 1 && b == 1 && c == 1

    var extra          = true
    var circleDetected = false
    var endDetected    = false
    Iterator.continually {
      val next = (a + b + c) % modBy
      a = b
      b = c
      c = next
      next
    }.takeWhile { e =>
      endDetected |= e == 0
      circleDetected |= seen(a, b, c)
      val stopAfterThis = endDetected || circleDetected
      val includeNext   = !stopAfterThis || extra
      if (stopAfterThis) {
        extra = false
      }
      includeNext
    }
  }

  val nonDivisors = Iterator.from(1, 2).filter { test =>
    !tribonacciMods(test).contains(0)
  }.take(124).toList.last
  println(nonDivisors)
}

@main def euler149(): Unit = {
  extension (i: Int) {
    def toBigInt = BigInt(i)
  }

  val cache = mutable.HashMap.empty[Int, Int]

  def laggedFibonacci(k: Int): Int = {
    def eval = {
      if (1 <= k && k <= 55) {
        ((100003.toBigInt - 200003.toBigInt * k + 300007L * k * k * k) % 1000000 - 500000).toInt
      } else if (56 <= k && k <= 4000000) {
        ((laggedFibonacci(k - 24) + laggedFibonacci(k - 55) + 1000000) % 1000000 - 500000)
      } else {
        throw new RuntimeException()
      }
    }

    cache.getOrElseUpdate(k, eval)
  }

  //val boxSize  = 4
  val boxSize  = 2000
  val sequence = {
    println("prep")
    1 to boxSize * boxSize map { k => laggedFibonacci(k) }
    //List(-2, 5, 3, 2, 9, -6, 5, 1, 3, 2, 7, 3, -1, 8, -4, 8)
  }

  def numberAt(x: Int, y: Int) = {
    sequence(x + y * boxSize)
  }

  def largestSubSum(sequence: Iterator[Int]) = {
    if (sequence.isEmpty) 0 else {
      var largestLeftSum = 0L
      var first          = true
      var totalMax       = Long.MinValue
      sequence.foreach { next =>
        if (first) {
          largestLeftSum = next
          first = false
        } else {
          largestLeftSum = (largestLeftSum + next) max next
        }
        totalMax = totalMax max largestLeftSum
      }
      totalMax
    }
  }

  def solution = {
    println("do")
    var largest = 0L

    def range = 0 until boxSize

    largest = range.map { colOrRow =>
      val first  = largestSubSum(range.iterator.map(xOrY => numberAt(colOrRow, xOrY)))
      val second = largestSubSum(range.iterator.map(xOrY => numberAt(xOrY, colOrRow)))
      first max second
    }.max

    largest = largest max (0 until 2 * boxSize).map { i =>
      val edgeX       = 0 max (i - boxSize)
      val mirrorEdgeX = boxSize - edgeX - 1
      val edgeY       = i min (boxSize - 1)

      def coordinatesSlash(startX: Int, startY: Int) = {
        Iterator.iterate((startX, startY))((x: Int, y: Int) => (x + 1, y - 1))
                .takeWhile((x, y) => x < boxSize && y >= 0)
      }

      def coordinatesBackSlash(startX: Int, startY: Int) = {
        Iterator.iterate((startX, startY))((x: Int, y: Int) => (x + 1, y + 1))
                .takeWhile((x, y) => x < boxSize && y < boxSize)
      }

      def allCoordinatesSlash = coordinatesSlash(edgeX, edgeY)

      def allCoordinatesBackSlash = coordinatesBackSlash(mirrorEdgeX, edgeY)

      val first  = largestSubSum(allCoordinatesSlash.map { (x, y) => numberAt(x, y) })
      val second = largestSubSum(allCoordinatesBackSlash.map { (x, y) => numberAt(x, y) })
      first max second
    }.max
    largest
  }

  println(solution)

}

