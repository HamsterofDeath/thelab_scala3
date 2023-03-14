package hod.other

object Vsauce2 {
  def main(args: Array[String]): Unit = {
    val solutions= 1 to 1000 filter { pages =>
      30+ 0.25*pages+0.125*pages == pages
    }
    println(solutions)
  }
}
