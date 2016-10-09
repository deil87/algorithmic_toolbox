package week5

import java.util.Scanner

/**
  * TIn this problem, your goal is to compute the length of a longest common subsequence of three sequences
  */

object LongestCommonSubsequence {

  def calculateEditDistance(str1: Array[Int], str2: Array[Int], distancesMtx: Array[Array[Int]]): Int = {

    val length1: Int = str1.length
    val length2: Int = str2.length
    for(i <- 0 to length1) distancesMtx(i)(0) = 0
    for(j <- 0 to length2) distancesMtx(0)(j) = 0

    println(" Before:")
    for(ni <- 0 to length1) println(distancesMtx(ni).mkString(" "))

    for(i <- 1 to length1) {
      for(j <- 1 to length2) {
        lazy val insertion = distancesMtx(i)(j - 1)
        lazy val deletion = distancesMtx(i - 1)(j)
        lazy val matched = distancesMtx(i -1 )(j - 1) + 1
        lazy val mismatched = distancesMtx(i - 1)(j - 1)

        val variants = List(insertion, deletion)
        val matchedMaxCount =
          if(str1(i -1) == str2(j -1))
            (matched :: variants).sortWith(_ > _).head
        else
            (mismatched :: variants).sortWith(_ > _).head

        distancesMtx(i)(j) = matchedMaxCount

      }
    }
    println(" After:")
       for(ni <- 0 to length1) println(distancesMtx(ni).mkString(" "))
    distancesMtx(length1)(length2)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val amountOfNumbers = s.nextInt()
    val numbers: Array[Int] = new Array[Int](amountOfNumbers)

    for (i <- 0 until amountOfNumbers) { numbers(i) = s.nextInt }

    val amountOfNumbers2 = s.nextInt()
    val numbers2: Array[Int] = new Array[Int](amountOfNumbers2)
    for (i <- 0 until amountOfNumbers2) { numbers2(i) = s.nextInt }


    val matchMtx: Array[Array[Int]] =  Array.ofDim[Int]( amountOfNumbers + 1, amountOfNumbers + 1)

    println(calculateEditDistance(numbers, numbers2, matchMtx))

  }

}
