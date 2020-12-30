package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharachters(s: String): Map[Char, Int] = {
    @tailrec
    def helper(remaing: List[Char], soFar: Map[Char, Int]): Map[Char, Int] = {
      if (remaing.isEmpty) soFar
      else if (soFar.keySet.contains(remaing.head)) helper(remaing.tail, soFar + (remaing.head -> (soFar(remaing.head) + 1)))
      else helper(remaing.tail, soFar + (remaing.head -> 1))
    }

    helper(s.toList, Map.empty)
  }

  def checkAnagrams(a: String, b: String): Boolean = 
    (countCharachters(a) -- countCharachters(b).keySet).isEmpty

  println(countCharachters("Scalaaaaaaallllllllasdadaawawa"))
  println(checkAnagrams("dessert", "stressed"))
  println(checkAnagrams("Scala", "Haskell"))
}
