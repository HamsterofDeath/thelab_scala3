package hod
package euler

import java.io.{EOFException, File}
import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.stream.Collectors
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, seqIsParallelizable}
import scala.collection.{Searching, mutable}
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.math.{BigInt, abs, max}
import scala.util.Random

@main def euler11(): Unit = {
  val data =
    """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
      |49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
      |81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
      |52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
      |22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
      |24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
      |32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
      |67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
      |24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
      |21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
      |78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
      |16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
      |86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
      |19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
      |04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
      |88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
      |04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
      |20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
      |20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
      |01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48""".stripMargin
  val grid = data.linesIterator.map(_.split(' ').map(_.toInt).toArray).toArray

  def valueAt(x: Int, y: Int) = {
    if (x > 19 || y > 19 || x < 0 || y < 0) 0 else grid(x)(y)
  }

  def topOf4(x: Int, y: Int) = {
    def productOf4(dirX: Int, dirY: Int) = {
      (0 to 3).map { i =>
        valueAt(x + dirX * i, y + dirY * i)
      }.product
    }

    productOf4(1, 0) max productOf4(1, 1) max productOf4(0, 1) max productOf4(-1, 1)
  }

  var best = 0
  0 to 19 foreach { x =>
    0 to 19 foreach { y =>
      val candidate = topOf4(x, y)
      if (candidate > best) {
        best = candidate
      }
    }
  }
  println(best)
}

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
  val txt = "resource/network.txt"
  val size = 40
  val originalNetwork = {
    var row = 0
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
    val covered = mutable.HashSet.empty[Int] += 0

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
  val initialCost = originalNetwork.map(_.map(_.getOrElse(0)).sum).sum / 2
  val optimizedCost = betterNetwork.map(_.cost).sum
  val saved = initialCost - optimizedCost
  println(initialCost)
  println(optimizedCost)
  println(saved)
}

@main def euler359(): Unit = {
  val fxr = 71328803586048L
  val limit = math.sqrt(fxr).toLong
  val factors = {
    Iterator.iterate(1L) {
      _ + 1
    }
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

  val A = 'A'
  val B = 'B'
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
  val lookUp = List.tabulate(lookupLimit)(stupidSequenceGenerate)

  val maxN = 17

  def nToIndexInSequence(n: Int) = {
    ((127 + 19 * n) * BigInt(7).pow(n)).bigInteger.longValueExact()
  }

  val test = false
  val a = if (test) "1415926535" else
    "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
  val b = if (test) "8979323846" else
    "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
  val elementSize = a.length

  def solveFor(nInInitialSequence: Long) = {
    @tailrec
    def solveForTuple(nInSequence: Long, sequenceIndex: Int): Boolean = {
      if (lookupLimit > sequenceIndex) {
        lookUp(sequenceIndex)(nInSequence.toInt) == A
      } else {
        val leftSubSequenceIndex = sequenceIndex - 2
        val isInLeftPart = nInSequence < fib(leftSubSequenceIndex)
        if (isInLeftPart) {
          solveForTuple(nInSequence, leftSubSequenceIndex)
        } else {
          val rightSubSequenceIndex = leftSubSequenceIndex + 1
          val newIndex = nInSequence - fib(leftSubSequenceIndex)
          solveForTuple(newIndex, rightSubSequenceIndex)
        }
      }
    }

    val index = indexOfNextLargerFib(nInInitialSequence)
    solveForTuple(nInInitialSequence, index)
  }

  def dab(n: Long): Int = {
    val indexOnCompressedSequence = n / elementSize
    val remainder = n % elementSize
    require(remainder.isValidInt)
    val isA = solveFor(indexOnCompressedSequence)
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
        val left = n / 10 match {
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
  val test = 1 to 20
  val solution = Iterator.from(1).find { e =>
    val candidate = e * 20L
    test.forall(candidate % _ == 0)
  }
  println(solution.get * 20)
}

@main def euler73(): Unit = {

  def isASmallerThanB(n1: Int, d1: Int, n2: Int, d2: Int) = n1 * d2.toLong < n2 * d1.toLong

  val ds = 1 to 12000
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
  val best = solution.toList.minBy(e => (2000000 - e._2).abs)._1
  println(best._1 * best._2)
}

@main def euler225(): Unit = {
  def tribonacciMods(modBy: Int) = {
    var a, b, c = 1

    def seen(a: Int, b: Int, c: Int) = a == 1 && b == 1 && c == 1

    var extra = true
    var circleDetected = false
    var endDetected = false
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
      val includeNext = !stopAfterThis || extra
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

  val cache = mutable.HashMap.empty[Int, Int]

  def laggedFibonacci(k: Int): Int = {
    def eval = {
      if (1 <= k && k <= 55) {
        ((100003.toBigInt - 200003.toBigInt * k + 300007L * k * k * k) % 1000000 - 500000).toInt
      } else if (56 <= k && k <= 4000000) {
        (laggedFibonacci(k - 24) + laggedFibonacci(k - 55) + 1000000) % 1000000 - 500000
      } else {
        throw RuntimeException()
      }
    }

    cache.getOrElseUpdate(k, eval)
  }

  //val boxSize  = 4
  val boxSize = 2000
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
      var first = true
      var totalMax = Long.MinValue
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
      val first = largestSubSum(range.iterator.map(xOrY => numberAt(colOrRow, xOrY)))
      val second = largestSubSum(range.iterator.map(xOrY => numberAt(xOrY, colOrRow)))
      first max second
    }.max

    largest = largest max (0 until 2 * boxSize).map { i =>
      val edgeX = 0 max (i - boxSize)
      val mirrorEdgeX = boxSize - edgeX - 1
      val edgeY = i min (boxSize - 1)

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

      val first = largestSubSum(allCoordinatesSlash.map { (x, y) => numberAt(x, y) })
      val second = largestSubSum(allCoordinatesBackSlash.map { (x, y) => numberAt(x, y) })
      first max second
    }.max
    largest
  }

  println(solution)

}

@main def euler10(): Unit = {
  val primes = allPrimesLong.takeWhile(_ <= 2000000)
  println(primes.sum)
}

@main def euler451(): Unit = {
  val max = 20000000

  val allNumbers = {
    bench("Shuffle")(Random().shuffle(Iterator.from(3).takeWhile(_ <= max).toArray))
  }

  val isPrime = bench("Prep")(dynamicPrimeCheck(max))

  def biggestSpecialModularInverseV2(n: Int) = {
    val max = n - 2
    var x = max.toLong

    while ((x * x) % n != 1) {
      x -= 1
    }
    x.toInt
  }

  val counter = AtomicInteger()
  bench("V2") {
    val (primes, test) = allNumbers.partition(isPrime(_))

    val precalculatedSolutions = dataReader("euler451").processAndClose(in => {
      val solutions = mutable.HashMap.empty[Int, Int]
      bench("Loading data... ") {
        try {
          while (true) {
            val x = in.readInt()
            val n = in.readInt()
            solutions.put(n, x)
          }
        } catch {
          case _: EOFException =>
        }
      }
      solutions
    })

    val sum = AtomicLong()
    val primeCount = primes.size
    println(primeCount + " primes")
    sum.addAndGet(primeCount)
    println(precalculatedSolutions.size + " solutions already known")
    sum.addAndGet(precalculatedSolutions.values.foldLeft(0L)(_ + _))

    val todo = test.filterNot(precalculatedSolutions.contains)
    val goal = todo.size
    println(goal + " solutions todo")
    val out = dataWriter("euler451", true)
    val time = AtomicLong(System.currentTimeMillis())
    todo.par.foreach { n =>
      val x = biggestSpecialModularInverseV2(n)
      val i = counter.incrementAndGet()
      val stepSize = 10000
      if (i % stepSize == 0) {
        val nju = System.currentTimeMillis()
        val diff = nju - time.getAndSet(nju)
        val speed = (stepSize.toDouble / diff) * 1000
        val remainingTime = (goal - i) / speed
        println(
          s"$i in $diff ms = $speed per second, remaining minutes: ${(remainingTime / 60).toInt}")
        out.flush()
      }
      out.doWithStream { out =>
        out.writeInt(x)
        out.writeInt(n)
      }
      sum.addAndGet(x)
    }

    out.close()

    println(sum.get())
  }

}

@main def euler452(): Unit = {

  def lazyCall(n: Int) = {
    val zero = BigInt(0)
    val one = BigInt(1)

    var leafs = 0L

    class Track {
      private val shiftFactor = (Math.log(n) / Math.log(2)).toInt + 1

      private val maxCount =
        Iterator.from(1).find(e => math.pow(shiftFactor, e) > Long.MaxValue).get - 1
      private val limit = Iterator.from(2).take(maxCount).foldLeft(1L)(_ * _)
      println(s"Shift factor is $shiftFactor, max depth: $maxCount, max n: $limit")
      require(limit >= n)

      private var encodedOccurences = 0L //BigInt(0)
      private var cursor = -1;
      private val data = Array.fill[Int](maxCount)(0)

      def currrentlyPilingUp: Int = {
        if (cursor >= 0) data(cursor) else Int.MaxValue
      }

      def push(n: Int): Unit = {
        if (currrentlyPilingUp > n) {
          encodedOccurences *= shiftFactor
          encodedOccurences += 1
          cursor += 1
          data(cursor) = n
        } else {
          encodedOccurences += 1
        }
      }

      def depth: Int = cursor

      def pop(): Unit = {
        encodedOccurences -= 1
        if (encodedOccurences % shiftFactor == 0) {
          cursor -= 1
          encodedOccurences /= shiftFactor
        }
      }

      def occurrences: Iterable[Int] = {
        new Iterable[Int] {
          override def iterator: Iterator[Int] = new Iterator[Int]:
            private var source = encodedOccurences

            override def hasNext: Boolean = source > 0

            override def next(): Int = {
              val ret = source % shiftFactor
              source /= shiftFactor
              ret.toInt
            }
        }
      }

      def uniqueHash: Long = encodedOccurences
    }

    case class Key(factors: Long, current: Int, hash: Long)
    val cache = mutable.HashMap.empty[Key, BigInt]

    val helper = Track()

    val permsCache = mutable.HashMap.empty[Number, BigInt]

    def fancyPermutationCount = {
      leafs += 1

      def eval = {
        def fromNBackwards(start: BigInt) = Iterator.iterate(start)(_ - 1)

        var product = one
        var slotsLeft = BigInt(n)
        helper.occurrences.foreach { count =>
          val smartNumerator = fromNBackwards(slotsLeft).take(count).product
          val smartDenominator = factorial(count)
          val factor = smartNumerator / smartDenominator
          slotsLeft -= count
          product *= factor
        }
        product
      }

      permsCache.getOrElseUpdate(helper.uniqueHash, eval)
    }

    def count(maxN: Int, maxFactor: Int): BigInt = {

      def eval = {
        val countDown = Range(maxFactor, 0, -1)

        var sum = zero
        var lastResult = zero
        var lastFactorA = 0
        var lastFactorB = 0
        countDown.foreach { use =>
          val remaining = maxN / use
          val additionalFactor = maxFactor / use
          val factorSwitched = remaining != lastFactorA ||
            additionalFactor != lastFactorB ||
            additionalFactor == 1
          lastFactorA = remaining
          lastFactorB = additionalFactor

          sum += {
            val subResult = {
              if (factorSwitched) {
                val newResult = {
                  val goDeeper = use > 1 && remaining >= 1
                  if (goDeeper) {
                    helper.push(use)
                    val actual = {
                      val newMaxFactor = use min remaining
                      count(remaining, newMaxFactor)
                    }
                    helper.pop()
                    actual
                  } else {
                    fancyPermutationCount
                  }
                }
                lastResult = newResult
              }
              lastResult
            }

            subResult
          }
        }
        sum
      }

      if (helper.depth > 0) {
        val key = Key(
          (maxN.toLong << 32) | maxFactor,
          helper.currrentlyPilingUp,
          helper.uniqueHash
        )
        cache.getOrElseUpdate(key, eval)
      } else {
        eval
      }
    }

    measured {
      count(n, n)
    }
  }

  println(lazyCall(1000000000) % 1234567891)
}

@main def euler655(): Unit = {

  def forEachPalindrome[U](digits: Int, allowZeroEnd: Boolean, cb: String => U): Unit = {
    val minRightSide = 0
    val isEven = digits % 2 == 0
    val maxRightSide = {
      val halfDigitCount = digits / 2
      ((if (isEven) 10.pow(halfDigitCount) else 10.pow(halfDigitCount + 1)) - 1).toIntSafe
    }
    val rightSideDigitCount = maxRightSide.digitCount
    minRightSide to maxRightSide foreach { rightSide =>
      if (allowZeroEnd || rightSide % 10 != 0) {
        val withLeadingZeroes =
          rightSide
            .toString
            .reverse
            .padTo(rightSideDigitCount, '0')
            .reverse
        val leftSide = {
          if (isEven)
            withLeadingZeroes.reverse
          else
            withLeadingZeroes
              .drop(1)
              .reverse
        }
        val palindrome = leftSide + withLeadingZeroes
        cb(palindrome)
      }
    }
  }

  val modBy = 10_000_019

  def countForDigits(n: Int) = {
    val outerPalindromeSize = if ((n / 2) % 2 == 0) n / 2 else n / 2 + 1
    val innerPalindromeSize = n - outerPalindromeSize
    val sectionADigits = outerPalindromeSize / 2
    val halfInnerSize = innerPalindromeSize / 2
    val sectionBDigits = {
      if (innerPalindromeSize % 2 == 0) halfInnerSize else halfInnerSize + 1
    }
    val sectionCDigits = halfInnerSize
    val sectionDDigits = outerPalindromeSize / 2
    val toBCFactor = 10.powSafe(sectionDDigits).bigInteger.longValueExact()
    val bcFactorMod = (toBCFactor % modBy).toIntSafe
    val toAFactor = 10.powSafe(sectionDDigits + sectionCDigits + sectionBDigits)
    val aFactorMod = toAFactor.mod(modBy).toIntSafe

    val lookup = Array.fill[Int](modBy)(0)

    forEachPalindrome(innerPalindromeSize, true, pd => {
      val mod = (((pd.toLong % modBy) * bcFactorMod) % modBy).toIntSafe
      lookup(mod) += 1
    })

    var totalCount = 0L
    forEachPalindrome(outerPalindromeSize, false, pd => {
      val sectionAMod = {
        (((pd.take(sectionADigits).toLong % modBy) * aFactorMod) % modBy).toIntSafe
      }
      val sectionDMod = {
        pd.takeRight(sectionDDigits).toInt
      }
      val sectionADMod = (sectionAMod + sectionDMod) % modBy
      val matchCount = lookup((modBy - sectionADMod) % modBy)
      totalCount += matchCount
    })
    totalCount
  }

  val solution = measured((4 to 32).reverse.par.map { d =>
    val ret = countForDigits(d)
    println(s"f($d) = $ret")
    ret
  }.sum)

  println(solution)
}

@main def euler135(): Unit = {
  val maxN = 1000000
  val progress = AtomicInteger(0)

  def countSolutionsForY(n: Int): Int = {
    if (progress.incrementAndGet() % 1000 == 0) print(".")
    var minZ = 1L
    var found = 0
    divisorsOf(n)
      .foreach { y =>
        var cursor = minZ
        var search = true
        while (search) {
          val z = cursor
          val diff = y - z
          val x = y + diff
          val nTest = x * x - y * y - z * z
          val good = nTest == n
          if (good) {
            minZ = minZ max z
            search = false
            found += 1
          } else {
            cursor += 1
            search = cursor <= y
          }
        }
      }
    found
  }

  measured {
    val solutions = Random.shuffle(1 until maxN)
      .par
      .filter(countSolutionsForY(_) == 10)
      .toList
      .sorted
    println()
    println("Count:" + solutions.size)
  }
}

@main def euler136(): Unit = {
  val maxN = 50000000
  val progress = AtomicInteger(0)

  def exactlyOneSolution(n: Int): Boolean = {
    if (progress.incrementAndGet() % 50000 == 0) print(".")
    var minZ = 1L
    var found = 0
    divisorsOf(n)
      .foreach { y =>
        var cursor = minZ
        var search = true
        while (search) {
          val z = cursor
          val diff = y - z
          val x = y + diff
          val nTest = x * x - y * y - z * z
          val good = nTest == n
          if (good) {
            minZ = minZ max z
            search = false
            found += 1
            if (found > 1) return false
          } else {
            cursor += 1
            search = cursor <= y
          }
        }
      }
    found == 1
  }

  def forNthMillion(n: Int): Long = {
    val filename = s"euler136_$n"

    val min = ((n - 1) * 1000000) max 1
    val maxEx = n * 1000000

    dataReader(filename).readObject[Option[Long]](Option.empty) match
      case Some(value) =>
        println(s"known value for $n is $value ($min until ${maxEx - 1})")
        value
      case None =>
        val saveMe = bench(s"Counting for $n") {
          val solutions: Long = {
            Random.shuffle(min until maxEx)
              .par
              .count(exactlyOneSolution)
          }
          println()
          println(s"Count for $n:" + solutions)
          solutions
        }
        dataWriter(filename, false).writeObject(Some(saveMe))
        println(s"saved $saveMe")
        saveMe
  }

  val total = (1 to (maxN / 1000000)).map(forNthMillion).sum
  println(s"Final solution: $total")

}

@main def euler139(): Unit = {
  var count = 0
  Pythagoras.foreachTriplet(100 * 1000000) { (a, b, c) =>
    val tileSize = b - a
    val tileable = c % tileSize == 0
    if (tileable) {
      count += 1
    }
  }
  println(count)
}

@main def euler196(): Unit = {
  def sumUntil(n: Int) = {
    val l = n.toLong
    if (l % 2 == 0)
      (l + 1) * (l / 2)
    else
      (l + 1) * ((l - 1) / 2) + l / 2 + 1
  }

  def valueAt(row: Int, col: Int) = {
    1 + sumUntil(row) + col
  }

  val rowsToTest = List(5678027, 7208785)

  val cache = ConcurrentHashMap[Long, Boolean].asScala

  def isAPrime(l: Long) = {
    cache.getOrElseUpdate(l, BigInt(l).isProbablePrime(10))
  }

  val allKnownTripletMembers = mutable.HashSet.empty[Long]

  rowsToTest.foreach { offByOneRow =>
    println()
    bench(s"prep $offByOneRow") {
      val rowToSumUp = offByOneRow - 1
      val rowsThatNeedPreparation = List(rowToSumUp - 1, rowToSumUp, rowToSumUp + 1)
      val partOfTriplets = ConcurrentHashMap[Long, Boolean].asScala
      rowsThatNeedPreparation.foreach { rowToPrepare =>
        def safePrimeValueAt(r: Int, c: Int) = {
          if (c < r && c >= 0) {
            val check = valueAt(r, c)
            if (isAPrime(check)) check else 0L
          } else {
            0
          }
        }

        (0 to rowToPrepare)
          .par
          .foreach { colToPrepare =>
            if (colToPrepare % 1000000 == 0) print('.')
            val triplet = mutable.HashSet.empty[Long]
            var primeCount = 0

            def isPartOfTriplet(r: Int, c: Int) = {
              def consider(value: Long) = {
                if (value > 0) {
                  primeCount += 1
                  triplet += value
                }
              }

              val check = safePrimeValueAt(r + 0, c + 0) > 0
              if (check) {
                consider(safePrimeValueAt(r - 1, c - 1))
                consider(safePrimeValueAt(r + 0, c - 1))
                consider(safePrimeValueAt(r + 1, c - 1))
                consider(safePrimeValueAt(r - 1, c + 0))
                consider(safePrimeValueAt(r + 0, c + 0))
                consider(safePrimeValueAt(r + 1, c + 0))
                consider(safePrimeValueAt(r - 1, c + 1))
                consider(safePrimeValueAt(r + 0, c + 1))
                consider(safePrimeValueAt(r + 1, c + 1))
              }
              primeCount >= 3
            }

            if (isPartOfTriplet(rowToPrepare, colToPrepare)) {
              triplet.foreach { value =>
                partOfTriplets.put(value, true)
              }
              partOfTriplets.put(valueAt(rowToPrepare, colToPrepare), true)
            }
          }
        allKnownTripletMembers ++= partOfTriplets.keys
      }
    }
  }

  val solution = rowsToTest.map { rowOffByOne =>
    val row = rowOffByOne - 1
    val relevant = (0 to row).iterator.map { col =>
      valueAt(row, col)
    }.filter(allKnownTripletMembers)
    (rowOffByOne, relevant.sum)
  }
  println(solution.map(_._2).sum)
}

@main def euler211(): Unit = {
  val limit = 64_000_000

  def f(n: Int) = divisorsOf(n).map(e => e * e).sum

  val progress = AtomicInteger()
  val solution = AtomicLong()
  (1 until limit)
    .par
    .foreach { e =>
      val counter = progress.incrementAndGet()
      if (counter % 1000000 == 0) {
        println(s"did $counter")
      }
      val fs = f(e)
      if (fs.isPerfectSquare) {
        println(s"$e > ${math.sqrt(fs)} (${divisorsOf(e).mkString(",")})")
        solution.getAndAdd(e)
      }
    }

  println(s"Solution: $solution")
}

@main def euler120(): Unit = {

  def remaindersOf(a: Int, modifier: Int) = {
    val base = BigInt(a + modifier)
    val divBy = a * a
    Iterator.from(1).map { n =>
      base.modPow(n, divBy)
    }
  }

  def remainders(a: Int): Iterator[Int] = {
    val divBy = a * a
    remaindersOf(a, -1)
      .zip(remaindersOf(a, 1))
      .map { (m, p) => ((m + p) % divBy).toIntSafe }
  }

  def max(a: Int) = {
    val marker = remainders(a).next()
    remainders(a).drop(1).takeWhile(_ != marker).max max marker
  }

  val maxs = 3 to 1000 map {
    max
  }

  println(maxs.sum)
}

@main def euler276(): Unit = {

  def foreachCoPrime[U](maxSum: Int)(cb: (Int, Int) => U): Unit = {
    def recur(m: Int, n: Int): Unit = {
      if (n + m <= maxSum) {
        cb(n, m)
        recur(2 * m - n, m)
        recur(2 * m + n, m)
        recur(m + 2 * n, n)
      }
    }

    recur(2, 1)
    recur(3, 1)
  }

  var count = 0L
  foreachCoPrime(200000) { (a, b) =>
    count += 1
  }
  println(count)

  val maxPerimeter = 100
  val maxSideLength = maxPerimeter / 3 + 1

  def iterateFrom(start: Int, last: Int) = Iterator.from(start).takeWhile(_ <= last)

  def solution = {
    iterateFrom(1, maxSideLength)
      .map { a =>
        val subSum = {
          iterateFrom(a, maxPerimeter / 2)
            .map { b =>
              val gcdTemp = gcdEuclid(a, b)
              iterateFrom(b, a + b)
                .filter(c => a + b + c <= maxPerimeter)
                .count(c => a + b > c && 1 == gcdEuclid(c, gcdTemp))
                .toLong
            }.sum
        }
        subSum
      }.sum
  }

  print(solution)
}

@main def euler86(): Unit = {
  def shortestPathIsInteger(a: Int, b: Int, c: Int): Boolean = {
    def lengthSqrOf(first: Int, second: Int) = first * first + second * second

    val path1 = lengthSqrOf(a + b, c)
    val path2 = lengthSqrOf(a + c, b)
    val shortest = path1 min path2
    shortest.isPerfectSquare
  }

  def count(m: Int) = {
    (1 to m).par.map { a =>
      (a to m).map { b =>
        (b to m).count { c =>
          shortestPathIsInteger(a, b, c)
        }
      }.sum
    }.sum
  }

  println(count(1817))
  println(count(1818))
}

@main def euler142(): Unit = {
  def findLowerNumberInTheMiddle(xpy: Long) = {
    allSquares.takeWhile(_ < xpy)
      .map(_.toLongSafe)
      .filter { xmy =>
        val y2 = xpy - xmy
        y2 % 2 == 0
      }.map { xmy => (xpy - xmy) / 2 }
  }

  val tuples = allSquares.map(_.toLongSafe)
    .flatMap { xpy =>
      findLowerNumberInTheMiddle(xpy)
        .map { y =>
          val x = xpy - y
          (x, y)
        }
    }
  val seenX = new mutable.HashMap[Long, mutable.Set[Long]]() with mutable.MultiMap[Long, Long]
  tuples.foreach { (x, y) =>
    if (seenX.contains(y)) {
      val zs = seenX(y)
      zs.foreach { z =>
        val xpz = (x + z).isPerfectSquare
        val xmz = (x - z).isPerfectSquare
        val ok = xpz && xmz
        if (ok) {
          println(x + y + z)
          System.exit(0)
        }
      }
    } else {
      seenX.addBinding(x, y)
    }
  }
}

@main def euler179(): Unit = {
  val limit = 10000000
  val divisorsCount = Array.fill(limit + 1)(0)

  for (i <- 1 to limit / 2) {
    for (j <- i * 2 to limit by i) {
      divisorsCount(j) += 1
    }
  }

  val result = (1 until limit).count(n => divisorsCount(n) == divisorsCount(n + 1))
  println(s"The number of integers 1 < n < $limit, for which n and n + 1 have the same number of positive divisors is: $result")
}

@main def euler187(): Unit = {
  val limit = 100000000
  val primes = allPrimes.takeWhile(_ <= limit).toVector
  var count = 0L
  var i = 0
  while (i < primes.length) {
    var j = i
    while (j < primes.length) {
      val composite = primes(i) * primes(j)
      if (composite < limit) {
        count += 1
      } else {
        j = primes.length // Break the inner loop since the composites will only increase.
      }
      j += 1
    }
    i += 1
  }

  println(s"There are $count composite integers n < ${limit} having precisely two, not necessarily distinct, prime factors.")
}

@main def euler129(): Unit = {
  def a(n: Int): Int = {
    var k = 1
    var x = 1

    while (x != 0) {
      x = (x * 10 + 1) % n
      k += 1
    }

    k
  }

  Iterator.from(1)
    .filter(i => gcdEuclid(i, 10) == 1)
    .map(i => i -> a(i))
    .tapEach(println)
    .find(_._2 > 1_000_000)
    .foreach { i =>
      println(i)
    }
}

@main def euler130(): Unit = {
  lazy val allPrimes = allPrimesLazy.takeWhile(_ <= 100_000_000).toSet

  def isPrime(i: Int) = {
    allPrimes(i)
  }

  def A(n: Int): Int = {
    var k = 1
    var r = 1
    while (r % n != 0) {
      r = (r * 10 + 1) % n
      k += 1
    }
    k
  }

  val target = 25
  var count = 0
  var n = 10
  var sum = 0

  while (count < target) {
    n += 1
    if (!isPrime(n) && gcdEuclid(n, 10) == 1) {
      val a = A(n)
      if ((n - 1) % a == 0) {
        count += 1
        sum += n
      }
    }
  }

  println(s"The sum of the first 25 composite values is $sum")
}



