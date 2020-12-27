package com.rockthejvm.numbers

import scala.annotation.tailrec
import com.rockthejvm.lists.RList
import com.rockthejvm.lists.RNil

object NumberOps {
  implicit class IntWrapper(n: Int) {
    def isPrime: Boolean = {
      // complexity O(sqrt(N))
      @tailrec
      def primeHelper(divisor: Int): Boolean = {
        if (divisor > math.sqrt(math.abs(n))) true
        else if (n % divisor == 0) false
        else primeHelper(divisor + 1)
      }
      if (n == 1 || n == -1 || n == 0) false
      else primeHelper(2)
    }

    def decompose: RList[Int] = {
      // complexity worse is O(sqrt(N)), best is O(logN)
      @tailrec
      def helper(remaining: Int, divisor: Int, accumulator: RList[Int]): RList[Int] = {
        if (divisor > math.sqrt(math.abs(remaining))) remaining :: accumulator
        else if (remaining % divisor == 0) helper(remaining / divisor, divisor, divisor :: accumulator)
        else helper(remaining, divisor + 1, accumulator)
      }
      helper(n, 2, RNil).reverse
    }
  }

  
}
object NumberProblems extends App {
  
  import NumberOps._
  println(42.isPrime)
  println(42.decompose)
  println(517935871.decompose)
}
