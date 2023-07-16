package hod.other

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.util.Random

object ZeroPlayerWordle {
  val allWords = {
    Random.shuffle(Files.readString(Paths.get("resource/words_alpha.txt"), StandardCharsets.UTF_8)
      .linesIterator
      .map(_.toLowerCase)
      .distinct
      .toList)
  }

  class Game {
    private val solutionWord = allWords.toVector(scala.util.Random.nextInt(allWords.size))
    private val potentialWords = allWords.filter(_.length == solutionWord.length)

    case class GuessWord(word: String, feedback: Long)

    def feedbackToLong(assumedSolution: String, guess: String): Long = {
      var feedback = 0L

      // Check for "contains here"
      for (i <- guess.indices) {
        if (guess(i) == assumedSolution(i)) {
          feedback |= (2L << (i * 2))
        }
      }

      // Check for "contains elsewhere"
      for (i <- guess.indices) {
        if ((feedback & (3L << (i * 2))) == 0 && assumedSolution.contains(guess(i))) {
          feedback |= (1L << (i * 2))
        }
      }
      feedback
    }

    def feedbackToLong(guess: String): Long = {
      feedbackToLong(solutionWord, guess)
    }

    def feedbackToString(feedback: Long, length: Int): String =
      (0 until length).map { i =>
        (feedback >>> (i * 2)) & 3 match {
          case 0 => "N"
          case 1 => "?"
          case 2 => "Y"
          case _ => throw new RuntimeException("Invalid feedback")
        }
      }.mkString

    var pool = {
      val totalSize = potentialWords.size
      println(s"Precalc ${totalSize}")
      var count = 0
      val percentCounter = totalSize / 100

      potentialWords.map { guess =>
        // update progress
        count += 1
        if (count >= percentCounter) {
          count = 0
        }

        GuessWord(guess, feedbackToLong(guess))
      }
    }

    def remainingAfterGuess(guess: String): List[GuessWord] = {
      val guessWord = pool.find(_.word == guess).get
      val (keep, drop) = pool.partition { candidate =>
        // Check if the candidate is still possible
        val candidateFeedback = feedbackToLong(candidate.word)
        val guessFeedback = guessWord.feedback

        (0 until guess.length).forall { i =>
          val candidateFeedbackHere = (candidateFeedback >>> (i * 2)) & 3
          val guessFeedbackHere = (guessFeedback >>> (i * 2)) & 3

          (guessFeedbackHere, candidateFeedbackHere) match {
            case (2, _) => guess(i) == candidate.word(i) // Right letter, right place
            case (1, 2) | (1, 0) => true // Right letter, wrong place
            case (0, _) => guess(i) != candidate.word(i) // Letter not in solution
            case _ => false
          }
        }
      }
      println(s"Cannot be a solution: ${drop.map(_.word).mkString(", ")}")
      keep
    }


    def selectBestGuess() = {
      println("Selecting best guess")
      val totalSize = pool.size
      var count = 0
      val percentCounter = totalSize / 100
      var needsLine = false
      val worstCaseRemaining = pool.par.map { guessWord =>
        // update progress
        count += 1
        if (percentCounter > 0 && count % percentCounter == 0) {
          print(".")
          count = 0
          needsLine = true
        }
        // simulate all possible feedbacks for the guess
        val possibleFeedbacks = pool.map { candidate =>
          (candidate, feedbackToLong(candidate.word, guessWord.word))
        }.groupBy(_._2)

        val maxWordsLeft = possibleFeedbacks.values.map(_.size).max
        (guessWord, maxWordsLeft)
      }
      if (needsLine) {
        println()
      }
      val targetScore = worstCaseRemaining.minBy(_._2)._2
      val options = worstCaseRemaining.filter(_._2 == targetScore)
      println(s"There are ${options.size} best options, choosing one at random")
      options(Random.nextInt(options.size))._1
    }

    def play(): Unit = {
      println(s"Chosen: $solutionWord")
      var rounds = 0
      while (pool.size > 1) {
        val guess = selectBestGuess()
        rounds += 1
        println(s"Guessed $guess, feedback is ${feedbackToString(guess.feedback, guess.word.length)}")

        val remaining = remainingAfterGuess(guess.word)
        pool = remaining

        println(s"There are ${pool.size} potential words left")
      }
      println(s"The word is ${pool.head.word}, found after $rounds guesses")
    }
  }

  def main(args: Array[String]): Unit = {
    new Game().play()
  }
}
