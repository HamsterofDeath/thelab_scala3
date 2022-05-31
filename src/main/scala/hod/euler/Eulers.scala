package hod.euler

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

@main def euler359 = {
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

@main def euler230 = {
  val cache = mutable.HashMap.empty[Int, Long]

  val A = 'A'
  val B = 'B'
  val ab = List(A,B)
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

  val lookupLimit      = 2
  val lookUp = List.tabulate(lookupLimit)(stupidSequenceGenerate)

  val maxN     = 17
  def nToIndexInSequence(n:Int) = {
    ((127 + 19 * n) * BigInt(7).pow(n)).bigInteger.longValueExact()
  }
  val test = false
  val a        = if (test) "1415926535" else "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
  val b        = if (test) "8979323846" else "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
  val elementSize = a.length

  def solveFor(nInInitialSequence: Long) = {
    def solveForTuple(nInSequence: Long, sequenceIndex: Int): Boolean = {
      if (lookupLimit>sequenceIndex) {
        lookUp(sequenceIndex)(nInSequence.toInt) == A
      } else {
        val leftSubSequenceIndex = sequenceIndex - 2
        val isInLeftPart         = nInSequence < fib(leftSubSequenceIndex)
        if (isInLeftPart) {
          solveForTuple(nInSequence, leftSubSequenceIndex)
        } else {
          val rightSubSequenceIndex = leftSubSequenceIndex + 1
          val newIndex                     = nInSequence - fib(leftSubSequenceIndex)
          solveForTuple(newIndex, rightSubSequenceIndex)
        }
      }
    }

    val index = indexOfNextLargerFib(nInInitialSequence)
    solveForTuple(nInInitialSequence, index)
  }

  def dab(n:Long):Int = {
    val indexOnCompressedSequence = n / elementSize
    val remainder = (n % elementSize)
    require(remainder.isValidInt)
    val isA = solveFor(indexOnCompressedSequence)
    val character = if(isA) {
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
        val index = nToIndexInSequence(n)-1
        BigInt(10).pow(n) * dab(index)
      }.sum

    }
  }
  println(solution)

}