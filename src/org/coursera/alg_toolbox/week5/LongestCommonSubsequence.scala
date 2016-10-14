package org.coursera.alg_toolbox.week5

import java.util.Scanner

/**
  * TIn this problem, your goal is to compute the length of a longest common subsequence of three sequences
  */

object LongestCommonSubsequence {

  def calculateEditDistance(str1: Array[Int], str2: Array[Int], str3: Array[Int], matchedMtrx: Array[Array[Array[Int]]]): Int = {

    val length1: Int = str1.length
    val length2: Int = str2.length
    val length3: Int = str3.length

    for(i <- 0 to length1) matchedMtrx(i)(0)(0) = 0
    for(j <- 0 to length2) matchedMtrx(0)(j)(0) = 0
    for(k <- 0 to length3) matchedMtrx(0)(0)(k) = 0

//    println(" Before:")
//    for(ni <- 0 to length1) println(matchedMtrx(ni).mkString(" "))

    for(i <- 1 to length1) {
      for(j <- 1 to length2) {
        for(k <- 1 to length3) {
          lazy val shift1 = matchedMtrx(i)(j - 1)(k)
          lazy val shift2 = matchedMtrx(i)(j)(k -1)
          lazy val shift3 = matchedMtrx(i -1 )(j)(k)

          lazy val matched = matchedMtrx(i - 1)(j - 1)(k - 1) + 1
          lazy val mismatched = matchedMtrx(i - 1)(j - 1)(k -1)

          val variants = List(shift1, shift2, shift3)
          val matchedMaxCount =
            if (str1(i - 1) == str2(j - 1) &&  str2(j - 1) == str3(k-1))
              (matched :: variants).sortWith(_ > _).head
            else
              (mismatched :: variants).sortWith(_ > _).head

          matchedMtrx(i)(j)(k) = matchedMaxCount
        }
      }
    }
//    println(" After:")
//       for(ni <- 0 to length1) println(matchedMtrx(ni).mkString(" "))
    matchedMtrx(length1)(length2)(length3)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val amountOfNumbers = s.nextInt()
    val numbers: Array[Int] = new Array[Int](amountOfNumbers)

    for (i <- 0 until amountOfNumbers) { numbers(i) = s.nextInt }

    val amountOfNumbers2 = s.nextInt()
    val numbers2: Array[Int] = new Array[Int](amountOfNumbers2)
    for (i <- 0 until amountOfNumbers2) { numbers2(i) = s.nextInt }

    val amountOfNumbers3 = s.nextInt()
    val numbers3: Array[Int] = new Array[Int](amountOfNumbers3)
    for (i <- 0 until amountOfNumbers3) { numbers3(i) = s.nextInt }

    val matchMtx =  Array.ofDim[Int]( amountOfNumbers + 1, amountOfNumbers2 + 1, amountOfNumbers3 + 1)

    println(calculateEditDistance(numbers, numbers2, numbers3, matchMtx))

  }

}
