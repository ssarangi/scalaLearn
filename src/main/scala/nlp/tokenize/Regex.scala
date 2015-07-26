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

import scala.annotation.tailrec
import scala.collection.mutable.{MutableList, Stack}
import scala.math.abs

trait Regex {
  def matches(stringToMatch: String): Boolean
}

object Regex {

  def compile(regex_string: String): Regex = {
    // Return a new instance of the Regex Class which can then be used to match
    val formatted_regex = formatRegex(regex_string)
    val postfix_regex = infix2postfix(formatted_regex, false)
    val postfix_tree = postfix2Tree(postfix_regex)
    val nfa = regex2NFA(postfix_tree)
    new RegexImpl(nfa)
  }

  private class RegexImpl(private val nfa_root: State) extends Regex {
    /**
     * Takes as input the string to be matched and returns a boolean value based on it
     * @param stringToMatch
     * @return
     */
    def matches(stringToMatch: String): Boolean = {
      evaluateNFAiterative(this.nfa_root, stringToMatch)
    }

    /**
     * Implement an NFA evaluation technique via recursion. However, this has the disadvantage of running out of stack
     * space.
     * @param root: Root state to begin matching on
     * @param stringToMatch:
     * @return Boolean: Whether a match was successful or not
     */
    def evaluateNFArecursive(root: State, stringToMatch: String): Boolean = {

      root match {
        case Match() => if (stringToMatch.isEmpty) true else false

        case Split(out1, out2) =>
          evaluateNFArecursive(out1, stringToMatch) | evaluateNFArecursive(out2, stringToMatch)

        case Consume(c, out) =>
          if (stringToMatch.isEmpty)
            false
          else if (c != stringToMatch.head)
            false
          else
            evaluateNFArecursive(out, stringToMatch.tail)

        case Placeholder(pointingTo) => evaluateNFArecursive(pointingTo, stringToMatch)
      }
    }

    class StateWithMatchedString(val state: State, val matchedString: String)

    /**
     * Implement the iterative version of the NFA evaluation technique
     * @param root: Root node of the NFA tree
     * @param stringToMatch:
     * @return Boolean: Whether a match was successful or not
     */
    def evaluateNFAiterative(root: State, stringToMatch: String): Boolean = {

      val stack = new Stack[StateWithMatchedString]
      stack.push(new StateWithMatchedString(root, stringToMatch))

      var found_match = false
      while (!stack.isEmpty && !found_match) {
        val curr_state = stack.pop
        curr_state.state match {
          case Match() =>
            if (curr_state.matchedString.isEmpty)
              found_match = true

          case Split(out1, out2) =>
            stack.push(new StateWithMatchedString(out1, curr_state.matchedString))
            stack.push(new StateWithMatchedString(out2, curr_state.matchedString))

          case Consume(c, out) =>
            if (c == curr_state.matchedString.head) {
              stack.push(new StateWithMatchedString(out, curr_state.matchedString.tail))
            }

          case Placeholder(pointingTo) =>
            stack.push(new StateWithMatchedString(pointingTo, curr_state.matchedString))
        }
      }

      found_match
    }
  }

  private val RegexOpPrecedence = Map({'(' -> 1},
                                      {'|' -> 2},
                                      {'.' -> 3},
                                      {'?' -> 4},
                                      {'*' -> 4},
                                      {'+' -> 4},
                                      {'^' -> 5})

  // Arithmetic Operator Precedence. Kept for testing purposes.
  private val ArithOpPrecedence = Map({ '^' -> -4 },
                                      { '(' -> 0 },
                                      { '/' -> 3 },
                                      { '*' -> 3 },
                                      { '+' -> 2 },
                                      { '-' -> 2 })

  private def getPrecedence(c: Char, OpPrecedence: Map[Char, Int]): Int = {
    val prec = OpPrecedence.getOrElse(c, 6)
    abs(prec)
  }

  private def getRightAssociativity(c: Char, OpPrecedence: Map[Char, Int]): Boolean = {
    val prec = OpPrecedence.getOrElse(c, 6)
    prec < 0
  }

  private def formatRegex(regex: String): String = {

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

    val stack = new Stack[Char]

    if (is_arith)
      infix2postfixInner(input, stack, ArithOpPrecedence, "")
    else
      infix2postfixInner(formatRegex(input), stack, RegexOpPrecedence, "")
  }

  abstract class RegexExpr

  // ., a, b (Any Literal)
  case class Literal(c: Char) extends RegexExpr

  // a|b
  case class Or(lhs: RegexExpr, rhs: RegexExpr) extends RegexExpr

  // ab. -> Concatenation of a & b. Need to find a better way to represent concatenation
  case class Concat(lhs: RegexExpr, rhs: RegexExpr) extends RegexExpr

  // a* -> Zero or more elements of a
  case class Repeat(expr: RegexExpr) extends RegexExpr

  // a+ -> One or more elements of a. This can be optimized to Concat(a, Repeat(a)) but for now I want to keep a separate
  // case class for it.
  case class Plus(expr: RegexExpr) extends RegexExpr

  def postfix2Tree(postfix: String): RegexExpr = {
    val stack = new Stack[RegexExpr]

    postfix.foreach {
      case '.' =>
        // Take out the top 2 expr and concatenate them
        val rhs = stack.pop
        val lhs = stack.pop
        val concat_expr = new Concat(lhs, rhs)
        stack.push(concat_expr)

      case '*' =>
        // Take the top expr out of the stack and repeat it
        val top_expr = stack.pop
        val repeat_expr = new Repeat(top_expr)
        stack.push(repeat_expr)

      case '+' =>
        // Take the top expr out of the stack and find 1 or more items from it
        val top_expr = stack.pop
        val plus_expr = new Plus(top_expr)
        stack.push(plus_expr)

      case '|' =>
        // Take out the Top 2 expressions and create an OR out of it
        val rhs = stack.pop
        val lhs = stack.pop
        val or_expr = new Or(lhs, rhs)
        stack.push(or_expr)

      case default =>
        val expr = new Literal(default)
        stack.push(expr)
    }

    // Make sure that the stack has just one element now which is the root
    require(stack.size == 1, "Only root element should be present at this time")
    stack.pop
  }

  abstract class State

  case class Consume(val c: Char, val out: State) extends State
  case class Split(val out1: State, val out2: State) extends State
  case class Placeholder(var pointingTo: State) extends State

  case class Match() extends State

  def regex2NFA(regexExpr: RegexExpr): State = regex2NFA(regexExpr, Match())

  /**
   * Create the NFA graph from the regex tree we got from the postfix form of the regex
   * @param regex: Regex tree root for the postfix form
   * @param nextState: Next state to transition to
   * @return State: The root of the NFA graph.
   */
  private def regex2NFA(regex: RegexExpr, nextState: State): State = {
    regex match {
      case Literal(c) => new Consume(c, nextState)
      case Concat(lhs, rhs) => regex2NFA(lhs, regex2NFA(rhs, nextState))
      case Or(lhs, rhs) => new Split(regex2NFA(lhs, nextState), regex2NFA(rhs, nextState))
      case Repeat(r) =>
        val placeholder = new Placeholder(null)
        val split = new Split(regex2NFA(r, placeholder), nextState)
        placeholder.pointingTo = split
        placeholder
      case Plus(r) => regex2NFA(Concat(r, Repeat(r)), nextState)
    }
  }
}