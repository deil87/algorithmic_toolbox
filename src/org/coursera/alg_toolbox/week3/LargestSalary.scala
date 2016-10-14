package org.coursera.alg_toolbox.week3

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

/**
  * As the last question of a successful interview, your boss gives you a few pieces of paperwith numbers on it
  * and asks you to compose a largest number from these numbers.
  * Theresulting number is going to be your salary, so you are very much interested in maximizingthis number
  */
object LargestSalary {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val amountOfNumbers = s.nextInt()
    val numbers: Array[Int] = new Array[Int](amountOfNumbers)
    var i: Int = 0

    while (i < amountOfNumbers) {
      {
        numbers(i) = s.nextInt
        i += 1
      }
    }


    def calcLargestSalary(numbers: Array[Int], accumulator: ArrayBuffer[Int]): ArrayBuffer[Int] = numbers match {
      case v if v.length == 0 => accumulator
      case v =>
        var maxDigit = Int.MinValue
        for(d <- numbers) {
          if(isGreaterOrEqual(d.toString, maxDigit.toString))
            maxDigit = d
        }
        val (before, after) = numbers.span(_ != maxDigit)
        accumulator += maxDigit
        calcLargestSalary(before ++ after.drop(1), accumulator)
    }

    def isGreaterOrEqual(digit: String, maxDigit: String): Boolean = {
      val digitFirst = digit + maxDigit
      val maxDigitFirst = maxDigit + digit
      digitFirst > maxDigitFirst
    }

    val largestSalary: ArrayBuffer[Int] = calcLargestSalary(numbers.sortWith(_ > _), ArrayBuffer.empty[Int])
    println( if(largestSalary.isEmpty) "0" else largestSalary.mkString(""))
  }


}
