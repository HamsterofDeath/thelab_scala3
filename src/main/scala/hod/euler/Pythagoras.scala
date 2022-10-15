package hod.euler

import scala.collection.mutable

object Pythagoras {

  def foreachTriplet[U](maxPerimeter: Int)(cb: (Int, Int, Int) => U):Unit = {
    // TODO optimize
    def primitivePythagoreanTriplets(limit: Int)(cbInner: (Int, Int, Int) => Unit): Unit = {
      var m = 2
      var c = 0
      while ( {
        c < limit
      }) {
        var n = 1
        while (n < m) {
          c = m * m + n * n
          if (c <= limit) {
            val a = m * m - n * n
            val b = 2 * m * n
            if (gcdEuclid(a,gcdEuclid(b,c)) == 1) {
              cbInner(a min b, b max a, c)
            }
          } else {
            n = m
          }
          n += 1
        }
        m += 1
      }
    }

    val maxC  = maxPerimeter // wrong but works
    val known = mutable.HashSet.empty[(Int, Int, Int)]
    primitivePythagoreanTriplets(maxC) { (a, b, c) =>
      val key = (a,b,c)
      val nju = !known(key)
      if (nju) {
        known += key
        Iterator.from(1)
                .takeWhile(_ * (a + b + c) < maxPerimeter)
                .foreach { factor =>
                  val fa    = a * factor
                  val fb    = b * factor
                  val fc    = c * factor
                  val small = fa + fb + fc < maxPerimeter
                  if (small) {
                    cb(fa, fb, fc)
                  }
                }
      }
    }
  }
}
