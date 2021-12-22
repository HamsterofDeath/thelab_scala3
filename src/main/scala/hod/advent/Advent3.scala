package hod.euler
package hod.advent

import java.io.File
import java.nio.file.Files
import java.util.stream.Collectors
import scala.collection.JavaConverters.*
import scala.collection.mutable

def data: List[String] = {
  Utils.loadFile(3, false)
}

val input  = data
val digits = input.head.length

def majorityAt(i: Int, input: Iterable[String] = input) = {
  val chars     = input.map(_.charAt(i))
  val zeroCount = chars.count(_ == '0')
  val oneCount  = chars.size - zeroCount

  def fallback = 1

  if (zeroCount > oneCount) 0 else if (zeroCount < oneCount) 1 else fallback
}

@main def solve(): Unit = {
  val gamma   = (0 until digits).map(majorityAt(_)).mkString
  val epsilon = gamma.map {
    case '0' => '1'
    case '1' => '0'
  }

  val solution = Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
  println(solution)
}

@main def solveLevel2(): Unit = {

  def findRating(searchForMajority: Boolean) = {
    val majorityFlagAsInt = if (searchForMajority) 1 else 0
    val pool              = mutable.ArrayBuffer.empty ++= input
    (0 until digits).foreach { index =>
      if (pool.size > 1) {
        val bit     = majorityAt(index, pool) == majorityFlagAsInt
        val bitChar = if (bit) '1' else '0'
        pool.filterInPlace(s => s.charAt(index) == bitChar)
      }
    }
    Integer.parseInt(pool.mkString, 2)
  }

  println(findRating(true) * findRating(false))
}

