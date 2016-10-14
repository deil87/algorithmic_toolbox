package org.coursera.alg_toolbox.week2

import java.util.Scanner

import scala.math.BigInt

/**
  * Greatest common divider using Euclidian algorithm
  */
object GCD {

  def main(args: Array[String]): Unit = {

    def calculateGCDfor(a: BigInt, b: BigInt): BigInt = {
      if (b == BigInt(0)) a
      else {
        if (a > b) calculateGCDfor(b, a - b * (a / b))
        else calculateGCDfor(a, b - a * (b / a))
      }
    }

    val s = new Scanner(System.in)
    val a = s.nextBigInteger()
    val b = s.nextBigInteger()

    println {calculateGCDfor(a, b)}
  }
}
