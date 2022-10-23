package hod.other

import java.io.FileReader
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.util
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.util.Random

import hod.euler.measured
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.collection.Searching

object FiveLetterWords {

  val aCode: Int = 'a'
  val letters    = ('a' to 'z').toArray

  def letterToBit(c: Char) = 1 << (c - aCode)
  def bitToLetter(code: Int) = {
    var checkBit = 1
    var index    = 0
    while ((code & checkBit) == 0) {
      checkBit <<= 1
      index += 1
    }
    letters(index)
  }
  def wordToCode(word: String) = {
    var code = 0
    word.foreach { c =>
      code |= letterToBit(c)
    }
    code
  }
  def charToArrayIndex(c: Char) = c - aCode
  def arrayIndexToChar(c: Int): Char = (c + aCode).toChar

  val allWords = String(
    Files.readAllBytes(
      Paths.get("resource/words_alpha.txt")),
    StandardCharsets.UTF_8)
    .linesIterator
    .filter(e => Integer.bitCount(wordToCode(e)) == 5)
    .filter(_.length == 5)
    .toArray

  private class Node(val letterBit: Int, val depth: Int) {
    private      val ownLetter = if (depth > 0) bitToLetter(letterBit) else '?'
    private var isWord         = false
    private var hasWords       = false
    private lazy val nodes     = Array
      .tabulate[Node](26)(e => Node(letterToBit(arrayIndexToChar(e)), depth + 1))
    def insert(word: String): Unit = {
      if (depth < 5) {
        hasWords = true
        val charToCheck = charToArrayIndex(word(depth))
        nodes(charToCheck).insert(word)
      } else {
        isWord = true
      }
    }

    def foreachExcept[U](code: Int, atLeast: Array[Char])(cb: ((Array[Char], Int) => U)): Unit = {
      foreachExcept(code, Array.tabulate[Char](5)(_ => ' '), 0, atLeast)(cb)
    }

    private def foreachExcept[U](code: Int, hack: Array[Char], hackCode: Int, atLeast: Array[Char])
                                (cb: (Array[Char], Int) => U): Unit = {
      if (depth > 0) {
        hack(depth - 1) = ownLetter
      }

      if (hasWords) {
        nodes.foreach { subNode =>
          def isAtLeast = {
            util.Arrays.compare(hack, 0, depth, atLeast, 0, depth) >= 0
          }

          if ((subNode.letterBit & code) == 0 && isAtLeast) {
            subNode.foreachExcept(code, hack, hackCode | letterBit, atLeast)(cb)
          }
        }
      } else if (isWord) {
        cb(hack, hackCode | letterBit)
      }
    }

    override def toString = s"$ownLetter"
  }

  def v1(): Unit = {
    val root = Node(0, 0)

    allWords.foreach { word =>
      root.insert(word)
    }

    println(s"Checking ${allWords.length} words")
    val counter  = AtomicInteger(0)
    val progress = AtomicInteger(0)
    measured {
      val service = Executors.newFixedThreadPool(30)
      allWords.foreach { firstWord =>
        service.submit(new Runnable {
          override def run(): Unit = {
            if (progress.incrementAndGet() % 100 == 0) {
              print('.')
            }
            val level1 = wordToCode(firstWord)
            root.foreachExcept(level1, firstWord.toCharArray) { (secondWord, level2) =>
              val level12 = level1 | level2
              root.foreachExcept(level12, secondWord) { (thirdWord, level3) =>
                val level123 = level12 | level3
                root.foreachExcept(level123, thirdWord) { (forthWord, level4) =>
                  val level1234 = level123 | level4
                  root.foreachExcept(level1234, forthWord) { (fifthWord, _) =>
                    counter.incrementAndGet()
                    //   println(s"$w1,$w2,$w3,$w4,$w5")
                  }
                }
              }
            }
          }
        })
      }
      service.shutdown()
      service.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
    }
    println("Done")
    println(counter.get())

  }

  def v2(): Unit = {
    measured {
      val count = AtomicInteger()

      def takeAfter(word: BitWord, words: IndexedSeq[BitWord]) = {
        words.search(word) match
          case Searching.Found(foundIndex) => words.drop(foundIndex)
          case Searching.InsertionPoint(insertionPoint) => words.drop(insertionPoint - 1)
      }

      case class BitWord(word: String, bits: Int) extends Comparable[BitWord] {
        def overlapsWith(other: BitWord) = (other.bits & bits) != 0
        override def compareTo(o: BitWord): Int = word.compareTo(o.word)
      }

      class WorkingSet(words: List[BitWord], code: Int, wordPool: IndexedSeq[BitWord], depth: Int) {

        def stepsLeft = 5 - depth

        def forked(nextWord: BitWord) = {
          WorkingSet(
            nextWord :: words,
            code | nextWord.bits,
            takeAfter(nextWord, wordPool)
              .filterNot(_.overlapsWith(nextWord)),
            depth + 1
          )
        }

        def nextLevelCandidates = {
          wordPool
        }

        def currentSolution = words.reverse.map(_.word)
      }

      val words = allWords.map(w => BitWord(w, wordToCode(w)))
      val graph = words.map { bw => bw -> words.filterNot(_.overlapsWith(bw)).toIndexedSeq }.toMap


      def recur(state: WorkingSet): Unit = {
        val stepsLeft = state.stepsLeft
        if (stepsLeft > 0) {
          state.nextLevelCandidates.foreach { nextWord =>
            recur(state.forked(nextWord))
          }
        } else {
          count.incrementAndGet()
          println(state.currentSolution)
        }
      }

      val service = Executors.newFixedThreadPool(30)
      words.foreach { start =>
        service.submit(new Runnable {
          override def run(): Unit = {
            val helper = WorkingSet(
              List(start),
              start.bits,
              takeAfter(start, graph(start)),
              1)
            recur(helper)
          }
        })
      }
      service.shutdown()
      service.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
      println(count.get())
    }

  }

  def main(args: Array[String]): Unit = {
    v2()
  }
}
