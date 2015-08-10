package nlp.tokenize.simple

/**
 * Created by sarangis on 8/9/15.
 */
class LineTokenizer(val txt: String) {
  val sentences = txt.split(".")
  val new_line_sentences = txt.split("\n")

  def split(separator: Char): Vector[String] = {
    txt.split(separator).toVector
  }
}
