package org.coursera.alg_toolbox.week5

import java.util.Scanner

import scala.collection.mutable

/**
  * Dynamic programming
  * You are given a primitive calculator that can perform the following three operations with the current number
  * x: multiply x by 2, multiply x by 3, or add 1 to x. Your goal is given a positive integer n, find the
  * minimum number of operations needed to obtain the number n starting from the number 1
  */

object PrimitiveCalculatorFastest {

  def findMinimumOfOperationsFor(memo: Array[Int], numberTarget: Int): Int = {

    for( number <- 2 to numberTarget) {
      memo(number) = {
        val nBy3Operation = if (number % 3 == 0) 1 + memo(number / 3) else Int.MaxValue
        val nBy2Operation =  if (number % 2 == 0) 1 + memo(number / 2) else Int.MaxValue
        val nMinusOneOperation = 1 + memo(number - 1)

        val minPossible = Math.min(Math.min(nBy3Operation,nBy2Operation),nMinusOneOperation)

        minPossible
      }
    }
    memo(numberTarget)
  }


  def main(args: Array[String]): Unit = {


      val s = new Scanner(System.in)

      val targetNumber = s.nextInt
//    BenchmarkHelper.time("main"){
      val memo = new Array[Int](targetNumber + 1)
      memo(1) = 0

      val amountOfOperations: Int = findMinimumOfOperationsFor(memo, targetNumber)

      println(amountOfOperations)
      var i = targetNumber
      val reconstraction = mutable.ArrayBuffer.empty[Int]
      reconstraction += targetNumber
//      BenchmarkHelper.time("recon") {
        while (i > 1) {
          lazy val next3: Int = i / 3
          lazy val next2: Int = i / 2
          lazy val next1: Int = i - 1
          val for3 = if (i % 3 == 0) memo(next3) else Int.MaxValue
          val for2 = if (i % 2 == 0) memo(next2) else Int.MaxValue
          val for1 = memo(next1)
          val variants = List((for3, 1), (for2, 2), (for1, 3))
          val min = variants.sortWith(_._1 < _._1).head
          if (min._2 == 1) {
            reconstraction += next3
            i = next3
          }
          else if (min._2 == 2) {
            reconstraction += next2
            i = next2
          }
          else {
            reconstraction += next1
            i = next1
          }
        }
//      }
      println(reconstraction.reverse.mkString(" "))
//    }
  }

}
