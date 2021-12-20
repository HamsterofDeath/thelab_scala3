package hod.euler
package hod.advent

import java.io.File
import java.nio.file.Files
import java.util.stream.Collectors
import collection.JavaConverters._

def input:List[(String, Int)] = {
  Files
    .lines(File("resource/advent2").toPath)
    .collect(Collectors.toList)
    .asScala
    .toList
    .map(_.split(' '))
    .map {
      case Array(command, value) => (command, value.toInt)
    }
}

@main def solveIt(): Unit = {
  var (x, y) = (0, 0);
  input.foreach {
    case ("forward", i) => x += i
    case ("up", i) => y -= i
    case ("down", i) => y += i
  }
  println(x * y)
}

@main def solveIt2(): Unit = {
  var (x, y, aim) = (0, 0, 0);
  input.foreach {
    case ("forward", i) =>
      x += i
      y += aim*i
    case ("up", i) => aim -= i
    case ("down", i) => aim += i
  }
  println(x * y)
}