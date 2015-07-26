import nlp.tokenize.Regex

/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Satyajit Sarangi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

/**
 * Created by sarangis on 7/12/15.
 */

object scalaLearnApp extends App {
  def main(): Unit = {
    // LinearRegressionExample.MultiVariableExample()
//    val postfix = RegExp.infix2postfix("ab*|c+d", false)
//    val postfix_tree = RegExp.postfix2Tree(postfix)
//    val nfa = RegExp.regex2NFA(postfix_tree)
//    val matches = RegExp.evaluateNFArecursive(nfa, "cccccccccccccccd")
//    val matches1 = RegExp.evaluateNFAiterative(nfa, "abcccccccccccccccd")
//    val a = 2
//    val b = 3
//    val c = a + b
    val compiled_regex = Regex.compile("ab*|c+d")
    val matches = compiled_regex.matches("cccccccccd")
  }

  main()
}
