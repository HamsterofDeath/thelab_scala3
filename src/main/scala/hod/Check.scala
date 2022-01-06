package hod.euler
package hod

@main def runIt = {
  def Er(Du: Int) = {
    var Ich = 2

    def Apfel = if Du % Ich == 0 then false else true

    while (Apfel == true) {
      //  println(Ich)
      Ich = Ich + 1
    }
    if (Ich == Du) then true
    else false
  }

  var Es = (Iterator.single(2) ++ Iterator.from(3).filter(Er)).take(1000)

  var zerlege = 1580
  var Durch = 2
  while (zerlege > 1) {
    while (zerlege % Durch == 0) {
      println(s"Teile $zerlege durch $Durch, rest ${zerlege / Durch}")
      zerlege = zerlege / Durch;
    }
    Durch = Es.next
  }
}
