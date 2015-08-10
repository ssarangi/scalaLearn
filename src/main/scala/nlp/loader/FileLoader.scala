package nlp.loader

import scala.io.Source

/**
 * Created by sarangis on 8/9/15.
 */
class FileLoader(filename: String) {
  val lines = Source.fromFile(filename).getLines().toVector
  val str = lines.mkString
}
