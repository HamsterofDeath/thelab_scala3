package hod.euler
package hod.euler

import scala.collection.mutable

@main def euler258(): Unit = {
  val seed = Array.fill[Int](2001)(1)

  val modBy  = 20092010
  var aIndex = 0
  var bIndex = 1

  var ones = 0
  def next() = {
    val nprn = (seed(aIndex) + seed(bIndex)) % modBy
    seed(aIndex) = nprn
    aIndex += 1
    aIndex %= 2000
    bIndex += 1
    bIndex %= 2000
    if (nprn==1) {
       ones += 1
    } else {
      ones = 0
    }
    nprn
  }

  def brute(n:Int):Int = {
    if (n<=1999) 1 else (brute(n-2000)+brute(n-1999)) % modBy
  }

  var last = 1
  var diff = 1
  Iterator.continually(next()).takeWhile(_ => ones < 2000).foreach { i =>
    if (i!=last && ones>0) {
      last = i
      diff += 1
      println(i + ", " + ones+", "+diff)
    }
  }



}

@main def euler10():Unit = {
  def isPrime(n:Int) = {
    val limit = Math.sqrt(n).toInt
    val tests = Iterator.single(2) ++ Iterator.from(3,2).takeWhile(_ <= limit)
    !tests.exists(n % _ == 0)
  }
  val primes =  Range(2,4).map(_.toLong) ++ (5 until 2000000 filter isPrime).map(_.toLong)
  println(primes.sum)
}
