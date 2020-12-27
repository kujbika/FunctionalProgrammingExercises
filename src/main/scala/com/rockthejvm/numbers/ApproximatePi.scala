package com.rockthejvm.numbers

import com.rockthejvm.lists.RList
import scala.util.Random


object ApproximatePi extends App {

  def approximate(numberOfPointsWithin: Int): Double =
    RList.from(1 to numberOfPointsWithin)
      .map { samplePoint => 
      val x = Random.nextDouble() * 2 - 1
      val y = Random.nextDouble() * 2 - 1
      x * x + y * y
    }.filter(_ <= 1.0)
      .length / numberOfPointsWithin.toDouble * 4.0

  println(s"Reference value: ${math.Pi}")
  // println(s"The value of Pi is approximately ${approximate(1000)}")
  // println(s"The value of Pi is approximately ${approximate(2000)}")
  // println(s"The value of Pi is approximately ${approximate(3000)}")
  // println(s"The value of Pi is approximately ${approximate(4000)}")
  println(s"The value of Pi is approximately ${approximate(100000)}")
}
