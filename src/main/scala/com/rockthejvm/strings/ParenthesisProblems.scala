package com.rockthejvm.strings

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

object ParenthesisProblems extends App {
  def hasValidParenthesis(s: String): Boolean = {
    @tailrec
    def helper(remaining: String, openParens: Int): Boolean = {
      if (remaining.isEmpty) openParens == 0
      else if (openParens == 0 && remaining.head == ")".head) false
      else if (remaining.head == "(".head) helper(remaining.tail, openParens + 1)
      else helper(remaining.tail, openParens - 1)
    }
    helper(s.filter(char => char == "(".head || char == ")".head), 0)
  }

  /*
   * n = 1 => List("()")  
   * n = 2 => List("()()", "(())")  
   */
  def generateAllValidParenthesis(n: Int): List[String] = {
    // first implementation: naive
    // we know it has to start with a (
    @tailrec
    def naive(remaining: Int, soFar: ParSeq[String]): ParSeq[String] = {
      if (remaining == 0) soFar
      else naive(remaining - 1, soFar.par.flatMap(pars => List(pars + "(", pars + ")")))
    }

    // sophisticated
    // from n = 1 to n = 2 
    // () + () = append
    // ( + () + ) = inject
    // () + () = prepend
    // => [(), (())]
    //
    @tailrec
    def sophisticated(remaining: Int, soFar: Set[String]): Set[String] = {
      if (remaining == 0) soFar
      else {
        val newStrings = for {
          string <- soFar
          index <- 0 until string.length
          } yield {
            val (before, after) = string.splitAt(index)
            s"$before()$after"
          }
          sophisticated(remaining - 1, newStrings.toSet)
      }
    }

    // naive(2 * n - 1, ParSeq("(")).filter(hasValidParenthesis).toList
    assert(n >= 0)
    if (n == 0) Nil
    else sophisticated(n - 1, Set("()")).toList
  }

  println(hasValidParenthesis("()()"))
  println(hasValidParenthesis("(()"))
  println(hasValidParenthesis(")("))
  println(hasValidParenthesis("()"))

  val currentTime = System.currentTimeMillis() 
  println(generateAllValidParenthesis(1))
  println(generateAllValidParenthesis(2))
  println(generateAllValidParenthesis(3))
  println(generateAllValidParenthesis(4))
  println(generateAllValidParenthesis(10))
  println(System.currentTimeMillis() - currentTime)
}
