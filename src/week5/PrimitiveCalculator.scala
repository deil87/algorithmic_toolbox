package week5

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.{Scanner, StringTokenizer}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.Breaks._

/**
  * Dynamic programming
  * You are given a primitive calculator that can perform the following three operations with the current number
  * x: multiply x by 2, multiply x by 3, or add 1 to x. Your goal is given a positive integer n, find the
  * minimum number of operations needed to obtain the number n starting from the number 1
  */

object PrimitiveCalculator {

  val memo = mutable.Map.empty[Int, Int]

  private def findMinimumOfOperationsFor(number: Int, countOfOperations: Int): Int = {
    println(s"Calculating For $number")
    if(number <= 1)
      countOfOperations
    else {
      memo.getOrElseUpdate(number, {
        val nBy3Operation = {
          lazy val n = number / 3
          if (number % 3 == 0) findMinimumOfOperationsFor(n, countOfOperations + 1) else Int.MaxValue
        }
        val nBy2Operation = {
          lazy val n = number / 2
          if (number % 2 == 0) findMinimumOfOperationsFor(n, countOfOperations + 1) else Int.MaxValue
        }
        val nMinusOneOperation = findMinimumOfOperationsFor(number - 1, countOfOperations + 1)
        //      println(s"$nBy3Operation: $nBy2Operation : $nMinusOneOperation")
        val minPossible = List((nBy3Operation, 1), (nBy2Operation, 2), (nMinusOneOperation, 3)).sortWith(_._1 < _._1).head
        //      println("minPossible:" + minPossible)
        val next =
          if (minPossible._2 == 1) number / 3
          else if (minPossible._2 == 2) number / 2
          else number - 1
        findMinimumOfOperationsFor(next, countOfOperations + 1)
      })
    }
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val targetNumber = s.nextInt

    val amountOfOperations:Int = findMinimumOfOperationsFor(targetNumber, 0)

    println("Amount of operations:" + amountOfOperations)
  }

}
