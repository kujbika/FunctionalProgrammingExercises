package com.rockthejvm.numbers

import scala.annotation.tailrec

object RecurringDecimals extends App {
  
  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = {

    def f2d(n: Long, d: Long): String = {

      @tailrec
      def findRecurrenceStart(digit: Long, digits: List[Long], remainder: Long, remainders: List[Long], currentIndex: Int): Int = {
        if (digits.isEmpty || remainders.isEmpty) -1
        else if (digit == digits.head && remainder == remainders.head) currentIndex
        else findRecurrenceStart(digit, digits.tail, remainder, remainders.tail, currentIndex + 1)
      }

      @tailrec
      def fractionDecimalsTailrec(num: Long, den: Long, digits: List[Long], remainders: List[Long]): String = {
        val quotient = (num * 10) / den
        val remainder = (num * 10) % den
        if (remainder == 0) (digits :+ quotient).mkString
        else {
          val recurrenceStartIndex = findRecurrenceStart(quotient, digits, remainder, remainders, 0)
          if (recurrenceStartIndex == -1) fractionDecimalsTailrec(remainder, den, digits :+ quotient, remainders :+ remainder)
          else {
            val (before, after) = digits.splitAt(recurrenceStartIndex)
            s"${before.mkString}(${after.mkString})"
          }
        }
      }
      if (n > 0 && d < 0) s"-${f2d(n, - 1 * d)}"
      if (n < 0 && d > 0) s"-${f2d(-1 * n, d)}"
      else {
        val quotient = n / d
        val remainder = n % d
        if (remainder == 0) quotient.toString
        else s"$quotient.${fractionDecimalsTailrec(remainder, d, Nil, Nil)}"
        
      }
    }
    f2d(numerator, denominator)

  }
  
  println(fractionToRecurringDecimals(1,3))
  println(fractionToRecurringDecimals(1,6))
  println(fractionToRecurringDecimals(1,7))
  println(fractionToRecurringDecimals(1,2003))
  println(fractionToRecurringDecimals(1,333))
}
