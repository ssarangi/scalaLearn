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
import scala.collection.mutable.{MutableList, Stack}
import scala.math.abs

object RegExp {

  val RegexOpPrecedence = Map({'(' -> 1},
                              {'|' -> 2},
                              {'.' -> 3},
                              {'?' -> 4},
                              {'*' -> 4},
                              {'+' -> 4},
                              {'^' -> 5})

  // Arithmetic Operator Precedence. Kept for testing purposes.
  val ArithOpPrecedence = Map({ '^' -> -4 },
                              { '(' -> 0 },
                              { '/' -> 3 },
                              { '*' -> 3 },
                              { '+' -> 2 },
                              { '-' -> 2 })

  def getPrecedence(c: Char, OpPrecedence: Map[Char, Int]): Int = {
    val prec = OpPrecedence.getOrElse(c, 6)
    abs(prec)
  }

  def getRightAssociativity(c: Char, OpPrecedence: Map[Char, Int]): Boolean = {
    val prec = OpPrecedence.getOrElse(c, 6)
    prec < 0
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
   * Implement the Shunting-Yard algorithm to convert a string from Infix form to Postfix form
   * @param input: Input string
   * @param is_arith: Boolean value indicating what operator precendence to use. This could be refactored to directly
   *                pass in the Operator precedence Map.
   * @return
   */
  def infix2postfix(input: String, is_arith: Boolean): String = {
    /**
     * The inner recursive function which implements the infix to postfix conversion. Uses tail recursion.
     * @param input: Input string
     * @param op_stack: Stack to store the operators coming in/
     * @param OpPrecedence: Precedence of operators. This varies between arithmetic, regular expressions and custom expressions.
     * @param postfix_prev_iter: postfix result from previous iteration
     * @return String: The postfix result from this iteration
     */
    def infix2postfixInner(input: String, op_stack: Stack[Char], OpPrecedence: Map[Char, Int], postfix_prev_iter: String): String = {
      var postfix = postfix_prev_iter
      var stack = op_stack

      if (input.isEmpty) return postfix + stack.toList.mkString

      val c = input.head

      c match {
        case '(' => stack.push(c)
        case ')' => {
          val stack_elems_to_pop = stack.takeWhile(_ != '(')
          postfix += stack_elems_to_pop.toList.mkString
          stack = stack.takeRight(stack.length - (stack_elems_to_pop.size + 1)) // + 1 is for the '(' itself. We want to remove it
        }
        case _ => {
          val cPrecedence = getPrecedence(c, OpPrecedence)
          val stack_to_take = stack.takeWhile(x => getPrecedence(x, OpPrecedence) >= cPrecedence && getRightAssociativity(x, OpPrecedence) == false)
          postfix += stack_to_take.toList.mkString
          stack = stack.takeRight(stack.length - stack_to_take.length)
          stack.push(c)
        }

      }

      infix2postfixInner(input.tail, stack, OpPrecedence, postfix)
    }

    // val formatted_regex = formatRegex("((a|b)*aba*)*(a|b)(a|b)")
    // val formatted_regex = "3+4*2/(1-5)^2^3"
    // val input = "5+((1+2)*4)-3"
    // val formatted_regex = "a*(m+(b/c))-d"

    val stack = new Stack[Char]

    if (is_arith)
      infix2postfixInner(input, stack, ArithOpPrecedence, "")
    else
      infix2postfixInner(formatRegex(input), stack, RegexOpPrecedence, "")
  }

  // Define the classes needed for conversion to NFA
  case class State(var c: Int, var out: State, var out1: State, var lastList: Int) {
    def this(c: Int) {
      this(c, null, null, 0)
    }

    def this(c: Int, out: State, out1: State) {
      this(c, null, null, 0)
    }
  }
  class Frag(val start: State, val out: MutableList[State])

  val Match = 256
  val Split = 257

  def postfix2NFA(input: String): State = {

    /**
     * Patch connects the dangling arrows in the pointer list l to the state s. It sets the outp = s for each pointer outp in l
     * @param l
     * @param s
     */
    def patch(l: MutableList[State], s: State): Unit = {
      l.transform(x => s)
    }

    // Main implementation of the Postfix to NFA
    val matchState = new State(Match)
    val stack = new Stack[Frag]

    input.foreach { x => x match {
        case '.' => {
          val e2 = stack.pop
          val e1 = stack.pop
          patch(e1.out, e2.start)
          stack.push(new Frag(e1.start, e2.out))
        }
        case '|' => {
          val e2 = stack.pop
          val e1 = stack.pop
          val s = new State(Split, e1.start, e2.start)
          stack.push(new Frag(s, e1.out ++ e2.out))
        }
        case '?' => {
          val e = stack.pop
          val s = new State(Split, e.start, null)
          stack.push(new Frag(s, e.out ++ MutableList(s.out1)))
        }
        case '*' => {
          val e = stack.pop
          val s = new State(Split, e.start, null)
          stack.push(new Frag(s, MutableList(s.out1)))
        }
        case '+' => {
          val e = stack.pop
          val s = new State(Split, e.start, null)
          patch(e.out, s)
          stack.push(new Frag(e.start, MutableList(s.out1)))
        }
        case _ => {
          val s = new State(11, null, null)
          stack.push(new Frag(s, new MutableList[State]))
        }
      }
    }

    val e = stack.pop
    patch(e.out, matchState)
    e.start
  }
}