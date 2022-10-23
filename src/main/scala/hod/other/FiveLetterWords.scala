package hod.other

import java.io.FileReader
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.util
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.util.Random

import hod.euler.{bench, measured}
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

  def v2(): Unit = {
    bench("Total") {
      val service = Executors.newFixedThreadPool(30)
      val count   = AtomicInteger()

      val allWords = bench("Load words") {
        String(
          Files.readAllBytes(
            Paths.get("resource/words_alpha.txt")),
          StandardCharsets.UTF_8)
          .linesIterator
          .filter(e => Integer.bitCount(wordToCode(e)) == 5)
          .filter(_.length == 5)
          .toArray
      }

      def takeAfter(word: BitWord, words: IndexedSeq[BitWord]) = {
        words.search(word) match
          case Searching.Found(foundIndex) => words.drop(foundIndex)
          case Searching.InsertionPoint(insertionPoint) => words.drop(insertionPoint)
      }

      case class BitWord(word: String, bits: Int) extends Comparable[BitWord] {
        def overlapsWith(other: BitWord) = (other.bits & bits) != 0
        override def compareTo(o: BitWord): Int = word.compareTo(o.word)
      }

      class WorkingSet(words: List[BitWord], code: Int, val wordPool: List[BitWord], depth: Int) {

        def recur(): Unit = {
          if (stepsLeft > 0) {
            forkEach(_.recur())
          } else {
            count.incrementAndGet()
            println(currentSolution)
          }
        }

        def stepsLeft = 5 - depth

        def forkEach(cb: WorkingSet => Unit) = {
          if (wordPool.nonEmpty) {
            wordPool.tails.foreach { tryWords =>
              if (tryWords.nonEmpty) {
                def doWork(): Unit = {
                  val nextWord = tryWords.head
                  val pool     = tryWords.filterNot(_.overlapsWith(nextWord))
                  cb(WorkingSet(
                    nextWord :: words,
                    code | nextWord.bits,
                    pool,
                    depth + 1
                  ))
                }

                if (depth == 0) {
                  service.submit(new Runnable {
                    override def run(): Unit = doWork()
                  })
                } else {
                  doWork()
                }
              }
            }
          }
        }

        def currentSolution = words.reverse.map(_.word)
        override def toString = s"WorkingSet($stepsLeft, $currentSolution)"
      }

      val words = bench("Bit fun") {allWords.map(w => BitWord(w, wordToCode(w)))}

      WorkingSet(
        Nil,
        0,
        words.toList,
        0).recur()

      service.shutdown()
      service.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
    }
  }

  def main(args: Array[String]): Unit = {
    v2()
  }
}
