package hod.euler
package hod.advent

import java.io.File
import java.nio.file.Files
import java.util.stream.Collectors
import collection.JavaConverters._

object Utils {
  def loadFile(i: Int, test:Boolean = false) = {
    val name = s"resource/advent${i}.${if (test)"test." else ""}txt"
    Files
      .lines(File(name).toPath)
      .collect(Collectors.toList)
      .asScala
      .toList
  }

}
