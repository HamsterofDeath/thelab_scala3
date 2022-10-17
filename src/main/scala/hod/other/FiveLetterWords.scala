package hod.other

import collection.parallel.CollectionConverters.ArrayIsParallelizable
import java.io.FileReader
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

import hod.euler.measured

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

    def foreachExcept[U](code: Int)(cb: ((Array[Char], Int) => U)): Unit = {
      foreachExcept(code, Array.tabulate[Char](5)(_ => ' '), 0)(cb)
    }

    private def foreachExcept[U](code: Int, hack: Array[Char], hackCode: Int)
                                (cb: (Array[Char], Int) => U): Unit = {
      if (depth > 0) {
        hack(depth - 1) = ownLetter
      }

      if (hasWords) {
        nodes.foreach { subNode =>
          if ((subNode.letterBit & code) == 0) {
            subNode.foreachExcept(code, hack, hackCode | letterBit)(cb)
          }
        }
      } else if (isWord) {
        cb(hack, hackCode | letterBit)
      }
    }

    override def toString = s"$ownLetter"
  }
  def main(args: Array[String]): Unit = {

    val root     = Node(0, 0)
    val allWords = String(
      Files.readAllBytes(
        Paths.get("resource/words_alpha.txt")),
      StandardCharsets.UTF_8)
      .linesIterator
      .filter(e => Integer.bitCount(wordToCode(e)) == 5)
      .filter(_.length == 5)
      .toArray

    allWords.foreach { word =>
      root.insert(word)
    }

    println(s"Checking ${allWords.length} words")
    val counter  = AtomicInteger(0)
    val progress = AtomicInteger(0)
    measured {
      Random.shuffle(allWords).toArray.par.foreach { w1 =>
        if (progress.incrementAndGet() % 100 == 0) {
          print('.')
        }
        val level1 = wordToCode(w1)
        root.foreachExcept(level1) { (secondWord, level2) =>
          val w2 = secondWord.mkString
          if (w1 < w2) {
            val level12 = level1 | level2
            root.foreachExcept(level12) { (thirdWord, level3) =>
              val w3 = thirdWord.mkString
              if (w2 < w3) {
                val level123 = level12 | level3
                root.foreachExcept(level123) { (forthWord, level4) =>
                  val w4 = forthWord.mkString
                  if (w3 < w4) {
                    root.foreachExcept(level123 | level4) { (fifthWord, _) =>
                      val w5 = fifthWord.mkString
                      if (w4 < w5) {
                        counter.incrementAndGet()
                        //   println(s"$w1,$w2,$w3,$w4,$w5")
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    println("Done")
    println(counter.get())
  }
}
