package hod

import java.io.{BufferedInputStream, BufferedOutputStream, BufferedReader, DataInputStream, DataOutputStream, EOFException, File, FileInputStream, FileOutputStream, FileReader, ObjectInputStream, ObjectOutputStream}
import java.math.{BigInteger, MathContext, RoundingMode}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

//noinspection ScalaWeakerAccess
package object euler {

  private val factorials = mutable.HashMap((0, BigInt.int2bigInt(1)))

  def singleDigitFactorial(digit: Long): Int = {
    digit match {
      case 0 => 1
      case 1 => 1
      case 2 => 2
      case 3 => 6
      case 4 => 24
      case 5 => 120
      case 6 => 720
      case 7 => 5040
      case 8 => 40320
      case 9 => 362880
    }
  }

  def factorial(n: Int): BigInt = {
    factorials.getOrElseUpdate(
      n, {
        n * factorial(n - 1)
      }
    )
  }

  def approximate(
                   whole: BigInt,
                   fractions: Iterator[Int],
                   terms: Int,
                   precision: Int
                 ): BigDecimal = {
    val mc = MathContext(precision, RoundingMode.HALF_UP)
    val one = BigDecimal(1, mc)
    val zero = BigDecimal(0, mc)

    def deepDive(remaining: Int): BigDecimal = {
      val next = BigDecimal(fractions.next(), mc)
      val step = {
        if (remaining > 0) {
          one / deepDive(remaining - 1)
        } else {
          zero
        }
      }
      next + step
    }

    BigDecimal(whole, mc) + (one / deepDive(terms))
  }

  def euler(terms: Int, precision: Int): BigDecimal = {
    val fractions =
      Iterator.from(1).flatMap(n => Iterator(1, n * 2, 1))
    approximate(2, fractions, terms, precision)
  }

  def sqrt2(terms: Int, precision: Int): BigDecimal = {
    val fractions = Iterator.continually(2)
    approximate(1, fractions, terms, precision)
  }

  def phi(terms: Int, precision: Int): BigDecimal = {
    val fractions = Iterator.continually(1)
    approximate(1, fractions, terms, precision)
  }

  def iterateLongs(from: Long): Iterator[Long] = {
    var cursor = from
    Iterator.continually {
      val ret = cursor
      cursor += 1
      ret
    }
  }

  def executionContextForThreads(
                                  threadCount: Int = Runtime.getRuntime.availableProcessors()
                                ): ExecutionContextExecutor =
    ExecutionContext
      .fromExecutor(Executors.newFixedThreadPool(threadCount))

  def measured[T](t: => T): T = {
    bench("Stuff")(t)
  }

  def bench[T](name: String)(t: => T): T = {
    println(s"Operation '$name' start")
    val start = System.nanoTime()
    val ret = t
    val end = System.nanoTime()
    val diffNanos = end-start
    val diffMillis = diffNanos / 1000000
    if(diffMillis>1000) {
      println(s"Operation '$name' took ${(end.toDouble - start) / 1000000000L} sec")
    } else {
      println(s"Operation '$name' took ${(end.toDouble - start) / 1000000L} msec")
    }

    ret
  }


  def countDivisorsOf(n: Long): Int = {
    var count = 0
    val limit = n / 2
    var test = 1

    while (test <= limit) {
      if (n % test == 0) {
        count += 1
      }
      test += 1
    }
    count
  }

  def divisorsOf(n: Long): Iterator[Long] = {
    properDivisorsOf(n) ++ Iterator.single(n)
  }

  def properDivisorsOf(n: Long): Iterator[Long] = {
    n match {
      case 0 | 1 => Iterator.empty
      case 2 => Iterator.single(1L)
      case _ =>
        val limit = math.sqrt(n).toLong
        var otherSide = List.empty[Long]
        Iterator.single(1L) ++ Iterator
          .from(2)
          .takeWhilePlusOne(_ < limit)
          .filter(n % _ == 0)
          .map { e =>
            if (n / e != limit) {
              otherSide = n / e :: otherSide
            }
            e.toLong
          } ++ otherSide.iterator
    }
  }

  def primeFactorsOf(n: Long): Iterator[Long] = {
    if (n == 0) Iterator.empty else {
      var remaining = n
      allPrimesLazy
        .iterator
        .takeWhilePlusOne(_ => remaining > 1)
        .flatMap { prime =>
          Iterator
            .continually(prime)
            .takeWhile(remaining % _ == 0)
            .map { _ =>
              remaining /= prime
              prime
            }
        }
    }
  }

  def primeFactorsOf(n: BigInt): Iterator[Long] = {
    if (n == 0) Iterator.empty else {
      var remaining = n
      allPrimesLazy
        .iterator
        .takeWhilePlusOne(_ => remaining > 1)
        .flatMap { prime =>
          Iterator
            .continually(prime)
            .takeWhile(remaining % _ == 0)
            .map { _ =>
              remaining /= prime
              prime
            }
        }
    }
  }

  def allPrimes: Iterator[Int] = {
    allPrimesLong.takeWhile(_ <= Int.MaxValue).map(_.toInt)
  }

  lazy val allPrimesLazy: LazyList[Long] = allPrimesLong.to(LazyList)

  def openOrCreateFile(name: String): File = {
    val f = File(s"resource/$name.data")
    if (!f.exists()) {
      val ok = f.createNewFile()
      require(ok, s"cannot create ${f.getAbsolutePath}")
    }
    f
  }

  def allPrimesLong: Iterator[Long] = {
    val cacheFile = openOrCreateFile("primes")

    var maxPrimeRead = 0L

    val fromFile: Iterator[Long] = {
      val in = DataInputStream(
        BufferedInputStream(FileInputStream(cacheFile), 1024 * 1024)
      )
      var row = 0

      def nextPrime = {
        val next = {
          try {
            Some(in.readLong())
          } catch {
            case _: EOFException => None
          }

        }
        row += 1
        if (next.isEmpty) {
          maxPrimeRead += 2
          None

        } else {
          val num = {
            next.get
          }
          require(num > maxPrimeRead, s"$num was <= $maxPrimeRead")
          maxPrimeRead = num
          Some(num)
        }
      }

      Iterator.continually(nextPrime).takeWhile(_.isDefined).map(_.get)
    }

    lazy val writer = {
      DataOutputStream(FileOutputStream(cacheFile, true))
    }

    def calculatedRemainingPrimes = {
      var next = 5L max maxPrimeRead

      def returnAndAddTwo = {
        val ret = next
        next += 2
        ret
      }

      println(s"switching to calculation mode at $next")
      Iterator
        .continually(returnAndAddTwo)
        .grouped(654321)
        .flatMap { chunk =>
          print('.')
          val subSet = {
            chunk.filter(_.isPrime)
          }
          subSet.foreach { prime =>
            writer.writeLong(prime)
          }
          writer.flush()
          subSet
        }
    }

    Iterator(2L, 3L) ++ fromFile ++ calculatedRemainingPrimes
  }

  def primes(n: Int): Iterator[Int] = {
    allPrimes.take(n)
  }

  def allSquares: Iterator[BigInt] = {
    Iterator
      .from(1)
      .map { e =>
        BigInt(e) * e
      }
  }

  def isReducedProperFraction(n: Long, d: Long): Boolean = gcdEuclid(n, d) == 1

  def dynamicPrimeCheck(preloadUntil: Long): Long => Boolean = {
    var max   = 0L
    val cache = mutable.HashSet.empty[Long]
    val func  = (n: Long) => {
      if (max < n) {
        cache ++= allPrimesLazy.dropWhile(_ <= max).takeWhile(_ <= n)
        max = n
      }
      cache(n)
    }
    func(preloadUntil)
    func
  }

  def gcdEuclid(a: Long, b: Long): Long = {
    var max       = Math.max(a, b)
    var min       = Math.min(a, b)
    var remainder = max % min
    while ( {
      remainder != 0
    }) {
      max = min
      min = remainder
      remainder = max % min
    }
    min
  }

  extension (f: File) {
    def slurp: Iterator[String] = {
      val in  = FileReader(f)
      val bin = BufferedReader(in, 4096)
      Iterator.continually(bin.readLine()).takeWhile { e =>
        val stop = e == null
        if (stop) bin.close()
        !stop
      }
    }

    def slurpWhole: String = slurp.mkString("\n")
  }

  extension[T] (it: IterableOnce[T]) {
    def occurences: mutable.HashMap[T, Int] = {
      val data = mutable.HashMap.empty[T, Int]
      it.iterator.foreach { e =>
        if (data.contains(e)) {
          data.put(e, data(e) + 1)
        } else {
          data.put(e, 1)
        }
      }
      data
    }
  }
  extension [T](i: Iterable[T]) {
    def allValuesDistinct: Boolean = i.toSet.size == i.size
  }

  extension (l: Long) {

    def sqr: Long = l * l

    def toIntSafe: Int = {
      require(l.toInt == l)
      l.toInt
    }

    def readable: String = {
      val sym = DecimalFormatSymbols()
      sym.setGroupingSeparator('.')
      val df = DecimalFormat("###,###,###,###", sym)
      df.format(l)
    }

    def allDigits: Iterator[Int] = l.toString.iterator.map(_.getNumericValue)

    def digitCount: Int = allDigits.size

    def pow(n: Int): Long = {
      n match {
        case 0 => 1
        case _ =>
          var ret = l
          var i = 1
          while (i < n) {
            ret *= l
            i += 1
          }
          ret
      }
    }

    def powSafe(n: Int): BigInt = {
      n match {
        case 0 => BigInt(1)
        case _ => BigInt(l).pow(n)
      }
    }

    def allDigitsReversed: Iterator[Int] = {
      var number = l
      Iterator
        .continually {
          val digit = number % 10
          number = number / 10
          digit.toInt
        }
        .takeWhilePlusOne { _ =>
          number > 0
        }
    }

    def sqrtPrecise(scale: Int): BigDecimal = {
      val mc =
        java.math.MathContext(scale + 1, java.math.RoundingMode.HALF_UP)
      BigDecimal.decimal(java.math.BigDecimal.valueOf(l).sqrt(mc), mc)
    }

    def sqrtNatural: Long = {
      java.math.BigInteger.valueOf(l).sqrt.longValueExact()
    }

    def isPerfectSquare: Boolean = {
      val n = l
      if (n < 0) {
        false
      } else {
        (n & 0x3f).toInt match {
          case 0x00 | 0x01 | 0x04 | 0x09 | 0x10 | 0x11 | 0x19 | 0x21 | 0x24 |
               0x29 | 0x31 | 0x39 =>
            var sqrt = 0L
            if (n < 410881L) { //John Carmack hack, converted to Java.
              // See: http://www.codemaestro.com/reviews/9
              var i = 0
              var x2 = .0f
              var y = .0f
              x2 = n * 0.5f
              y = n
              i = java.lang.Float.floatToRawIntBits(y)
              i = 0x5f3759df - (i >> 1)
              y = java.lang.Float.intBitsToFloat(i)
              y = y * (1.5f - (x2 * y * y))
              sqrt = (1.0f / y).toLong
            } else { //Carmack hack gives incorrect answer for n >= 410881.
              sqrt = Math.sqrt(n).toLong
            }
            sqrt * sqrt == n
          case _ =>
            false
        }
      }
    }

    def isPerfectSquareSlow: Boolean = {
      l != 0 && {
        val ref = java.math.BigInteger.valueOf(l)
        val sqrt = ref.sqrt
        sqrt.pow(2) == ref
      }
    }

    def isPrime: Boolean = {
      if (l % 2 == 0) return l == 2
      if (l % 3 == 0) return l == 3
      val sqrtN = Math.sqrt(l)
      var j = 5
      var step = 4
      while (j <= sqrtN) {
        if (l % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }
  }

  extension (i: Int) {
    def toBigInt = BigInt(i)
    def isPrime: Boolean = {
      if (i % 2 == 0) return i == 2
      if (i % 3 == 0) return i == 3
      val sqrtn = Math.sqrt(i)
      var j = 5
      var step = 4
      while (j <= sqrtn) {
        if (i % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }

    def readable: String = {
      i.toLong.readable
    }

  }


  extension[T] (it: Iterator[T]) {

    def memoizedByIndex: Int => T = {
      val cache = mutable.HashMap.empty[Int, T]
      var maxEvaluated = -1
      (i: Int) => {
        assert(i >= 0, "int overflow")
        while (i > maxEvaluated) {
          val t = it.next()
          maxEvaluated += 1
          cache.put(maxEvaluated, t)
        }
        cache(i)
      }
    }

    def takeWhilePlusOne(filter: T => Boolean): Iterator[T] = stopAfter(e => !filter(e))

    def stopAfter(isLastAccepted: T => Boolean): Iterator[T] = {
      var stop = false
      it.takeWhile { e =>
        val take = !stop
        stop |= isLastAccepted(e)
        take
      }
    }

    def lastElement: T = {
      var ret: T = null.asInstanceOf[T]
      while (it.hasNext) {
        ret = it.next()
      }
      ret
    }
  }

  extension (bi: BigInt) {

    def getDigitCount: Int = {
      val factor = Math.log(2) / Math.log(10)
      val digitCount = (factor * bi.bitLength + 1).toInt
      if (BigInteger.TEN.pow(digitCount - 1).compareTo(bi) > 0)
        return digitCount - 1
      digitCount
    }

    def allDigitsReversed: Iterator[Int] = digits.toArray.reverseIterator

    def digits: Iterator[Int] = bi.toString().iterator.map(Character.getNumericValue)

    def sqrtNatural: BigInt = BigInt(bi.bigInteger.sqrt)

    def isPerfectSquare: Boolean = {
      val java = bi.bigInteger
      val sqrt = java.sqrt
      (sqrt multiply sqrt) == java
    }

    def sqrt(scale: Int): BigDecimal = {
      val mc = MathContext(scale + 1, java.math.RoundingMode.HALF_UP)
      BigDecimal.decimal(
        java.math.BigDecimal(bi.bigInteger, mc).sqrt(mc),
        mc
      )
    }

    def toBigDecimal: BigDecimal = {
      BigDecimal(java.math.BigDecimal(bi.bigInteger))
    }

    def toIntSafe: Int = {
      bi.bigInteger.intValueExact()
    }
    def toLongSafe: Long = {
      bi.bigInteger.longValueExact()
    }

  }

  extension (bd: BigDecimal) {

    def fractionalPart: BigDecimal = {
      val whole = bd.toBigInt
      bd - BigDecimal(whole)
    }

    def continuedFractions: Iterator[Long] = {
      var remaining = bd

      def nextWhole: Long = remaining.toLong

      Iterator.continually {
        val nextValue = nextWhole
        remaining = {
          val num = BigDecimal(1, bd.mc)
          val denom = remaining - BigDecimal(nextValue, bd.mc)
          num / denom
        }
        nextValue
      }
    }

    def convergentFractions: Iterator[(BigInt, BigInt)] = {
      val fractions = continuedFractions.memoizedByIndex
      val forDenominator = (i: Int) => fractions(i + 1)
      val cache = mutable.HashMap.empty[(Int, Boolean), BigInt]

      def eval(n: Int, isNumerator: Boolean): BigInt = {
        cache.getOrElseUpdate(
          (n, isNumerator), {
            val on = if (isNumerator) fractions else forDenominator
            n match {
              case larger if larger > 2 =>
                val a = on(larger - 2)
                val b = eval(larger - 1, isNumerator)
                val c = eval(larger - 2, isNumerator)
                a * b + c
              case 2 => on(0)
              case 1 => BigInt(1)
              case _ => throw IllegalArgumentException(n.toString)
            }
          }
        )
      }

      Iterator.from(1).map { n =>
        val num = eval(n + 1, isNumerator = true)
        val denom = eval(n, isNumerator = false)
        (num, denom)
      }
    }

  }

  extension (bi: BigInteger) {

    def isPerfectSquare: Boolean = {
      val root = bi.sqrt
      (root multiply root) == bi
    }
  }

  extension (d: Double) {

    def isPerfectSquare: Boolean = {
      d != 0 &&
        math.sqrt(d).isNatural
    }

    def sqrt: Double = math.sqrt(d)

    def sqrtPrecise(scale: Int): BigDecimal = {
      java.math.BigDecimal
        .valueOf(d)
        .sqrt(java.math.MathContext(scale, java.math.RoundingMode.HALF_UP))
    }

    def isNatural: Boolean = {
      d.floor == d &&
        !d.isNaN &&
        !d.isInfinity
    }

  }

  extension[T] (o: Option[T]) {
    def openOrEval(excuse: => String): T = {
      o match {
        case Some(x) => x
        case None => throw RuntimeException(excuse)
      }
    }

    def openOr(excuse: String): T = {
      o match {
        case Some(x) => x
        case None => throw RuntimeException(excuse)
      }
    }
  }

  def readerWriter[T](name: String, eval: => T): T = {
    dataReader(name).readObject {
      val calculated = eval
      val writer = dataWriter(name, false)
      writer.writeObject(calculated)
      writer.close()
      calculated
    }
  }

  def dataReader(name: String): DataReader = {
    val file = openOrCreateFile(s"stream_$name")
    lazy val stream = DataInputStream(BufferedInputStream(FileInputStream(file)))
    new DataReader :
      override def processAndClose[T](cb: DataInputStream => T): T = {
        val ret = cb(stream)
        stream.close()
        ret
      }
  }

  def dataWriter(name: String, append: Boolean): DataWriter = {
    val es = Executors.newSingleThreadExecutor()
    val file = openOrCreateFile(s"stream_$name")
    val stream = DataOutputStream(BufferedOutputStream(FileOutputStream(file, append)))

    def synced[T](logic: => T) = {
      es.submit(new Runnable {
        override def run(): Unit = {
          logic
        }
      })
    }

    val ret = new DataWriter :

      override def doWithStream[T](cb: DataOutputStream => T): Unit =
        synced(cb(stream))

      override def flush(): Unit =
        synced(stream.flush())

      override def close(): Unit =
        synced {
          stream.close()
        }
        es.shutdown()
        es.awaitTermination(Long.MaxValue, TimeUnit.DAYS)

    ret
  }

  trait DataWriter {
    def doWithStream[T](cb: DataOutputStream => T): Unit

    def flush(): Unit

    def close(): Unit

    def writeObject[T](t: T): Unit = {
      doWithStream { dos =>
        val stream = new ObjectOutputStream(dos)
        stream.writeObject(t)
        stream.close()
        close()
      }
    }
  }

  trait DataReader {
    def processAndClose[T](cb: DataInputStream => T): T

    def readObject[T](default: => T): T = {
      processAndClose[T] { dis =>
        try {
          new ObjectInputStream(dis).readObject().asInstanceOf[T]
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            default
        }
      }
    }
  }
}

