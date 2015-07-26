package nlp.tokenize

import org.scalatest.FunSuite

/**
 * Created by sarangis on 7/26/15.
 */
class Regex$Test extends FunSuite {
  test("Regex works correctly") {
    val regex_compiled = Regex.compile("ab*|c+d")
    assert(regex_compiled.matches("cccccccccccccccd") == true)
    assert(regex_compiled.matches("abbbbb") == true)
    assert(regex_compiled.matches("abcccccccccccccccd") == false)
  }
}
