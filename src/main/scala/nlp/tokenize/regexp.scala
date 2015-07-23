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

package nlp.tokenize

/**
 * Created by sarangis on 7/21/15.
 */

//import scala.collection.parallel.mutable
//import scala.util.parsing.combinator.RegexParsers
//
//abstract class RegexExpr
//
//// ., a, b
//case class Literal(c: Char) extends RegexExpr
//// ab -> Concat(a, b); abc -> Concat(a, Concat(b, c))
//case class Concat(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
//// a|b
//case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
//// a*
//case class Repeat(expr: RegexExpr) extends RegexExpr
//// a+
//case class Plus(expr: RegexExpr) extends RegexExpr
//
//object RegexParser extends RegexParsers {
//  def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ {
//    char => Literal(char.head)
//  }
//
//  def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
//  def lit: Parser[RegexExpr] = charLit | parenExpr
//  def repeat: Parser[RegexExpr] = lit <~ "*" ^^ { case l => Repeat(l)}
//  def plus: Parser[RegexExpr] = lit <~ "+" ^^ { case p => Plus(p) }
//  def lowExpr: Parser[RegexExpr] = repeat | plus | lit
//
//  def concat: Parser[RegexExpr] = rep(lowExpr) ^^ { case list => listToConcat(list) }
//  def midExpr: Parser[RegexExpr] = concat | lowExpr
//
//  def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ { case l ~ "|" ~ r => Or(l, r) }
//  def highExpr: Parser[RegexExpr] = or | midExpr
//
//  def listToConcat
//}
//
//abstract class State
//
//class Consume(val c: Char, val out: State) extends State
//class Split(val out1: State, val out2: State) extends State
//case class Match() extends State
//
//class PlaceHolder(var pointingTo: State) extends State
//
//object NFA {
//  def regexToNFA(regex: RegexExpr): State = regexToNFA(regex, Match())
//
//  private def regexToNFA(regex: RegexExpr, andThen: State): State = {
//    regex match {
//      case Literal(c) => new Consume(c, andThen)
//      case Concat(first, second) => regexToNFA(first, regexToNFA(second, andThen))
//      case Or(l, r) => new Split(regexToNFA(l, andThen), regexToNFA(r, andThen))
//      case Repeat(r) => {
//        val placeholder = new PlaceHolder(null)
//        val split = new Split(regexToNFA(r, placeholder), andThen)
//        placeholder.pointingTo = split
//        andThen
//      }
//      case Plus(r) => regexToNFA(Concat(r, Repeat(r)), andThen)
//    }
//  }
//}
//
//object NFAEvaluator {
//  def evaluate(nfa: State, input: String): Boolean =
//    evaluate(Set(nfa), input)
//
//  def evaluate(nfas: Set[State], input: String): Boolean = {
//    input match {
//      case "" =>
//        evaluateStates(nfas, None).exists(_ == Match())
//      case string =>
//        evaluate(
//          evaluateStates(nfas, input.headOption),
//          string.tail
//        )
//    }
//  }
//
//  def evaluateStates(nfas: Set[State],
//                     input: Option[Char]): Set[State] = {
//    val visitedStates = mutable.Set[State]()
//    nfas.flatMap { state =>
//      evaluateState(state, input, visitedStates)
//    }
//  }
//
//  def evaluateState(currentState: State, input: Option[Char],
//                    visitedStates: mutable.Set[State]): Set[State] = {
//
//    if (visitedStates contains currentState) {
//      Set()
//    } else {
//      visitedStates.add(currentState)
//      currentState match {
//        case placeholder: Placeholder =>
//          evaluateState(
//            placeholder.pointingTo,
//            input,
//            visitedStates
//          )
//        case consume: Consume =>
//          if (Some(consume.c) == input
//              || consume.c == '.') {
//            Set(consume.out)
//          } else {
//            Set()
//          }
//        case s: Split =>
//          evaluateState(s.out1, input, visitedStates) ++
//          evaluateState(s.out2, input, visitedStates)
//        case m: Match =>
//          if (input.isDefined) Set() else Set(Match())
//      }
//    }
//  }
//}
//
//object Regex {
//  def fullMatch(input: String, pattern: String) = {
//    val parsed = RegexParser(pattern).getOrElse(
//      throw new RuntimeException("Failed to parse regex")
//    )
//    val nfa = NFA.regexToNFA(parsed)
//    NFAEvaluator.evaluate(nfa, input)
//  }
//
//  def matchAnywhere(input: String, pattern: String) =
//    fullMatch(input, ".*" + pattern + ".*")
//}
//
//object session {
//  Regex.fullMatch("aaaaab", "a*b") // True
//  Regex.fullMatch("aaaabc", "a*b") // False
//  Regex.matchAnywhere("abcde", "cde") // True
//}

import scala.annotation.tailrec

object RegExp {

  val OpPrecedence = Map({'(' -> 1},
                         {'|' -> 2},
                         {'.' -> 3},
                         {'?' -> 4},
                         {'*' -> 4},
                         {'+' -> 4},
                         {'^' -> 5})

  def getPrecedence(c: Char): Int = {
    OpPrecedence.getOrElse(c, 6)
  }

  def formatRegex(regex: String): String = {
    /**
     * Turn a+(b*c) to a+.(b*.c)
     * @param regex
     * @return
     */
    @tailrec def formatRegexInner(regex: String, result: String): String = {
      val allOperators = Array('|', '?', '+', '*', '^')
      val binaryOperators = Array('^', '|')

      if (regex.isEmpty) return result

      val c1 = regex.head
      val c2 = if (!regex.tail.isEmpty) regex.tail.charAt(0) else ' '

      val tmp = (if ((c1 != '(') && (c2 != ')') && (c2 != ' ') && !allOperators.contains(c2) && !binaryOperators.contains(c1)) "." else "")
      formatRegexInner(regex.tail, result + c1 + tmp)
    }

    formatRegexInner(regex, "")
  }

  /**
   * Implement the Shunting-Yard algorithm
   * @param input
   * @return
   */
  def infix2postfix(input: String): String = {
    ""
  }
}