package org.coursera.alg_toolbox.week5

import java.util.Scanner

/**
  * This problem is about implementing an algorithm for the knapsack without repetitions problem
  */

object KnapsackWithoutRepetitionProblem {

  def calculateMaxWeightOfKnapsack(capacity: Int, n: Int, weights: Array[Int], maxWeights: Array[Array[Int]]): Int = {

    for(ni <- 0 to n) maxWeights(ni)(0) = 0
    for(wi <- 0 to capacity) maxWeights(0)(wi) = 0

    for( ni <- 1 to n) {
      for( ci <- 1 to capacity) {
        val weightOfItem = weights(ni - 1)
        if(weightOfItem <= ci) {
          val weightWithNth = maxWeights(ni - 1)(ci - weightOfItem) + weightOfItem
          val weightWithoutNth = maxWeights(ni - 1)( ci)
          val maxWeight = Math.max(weightWithNth, weightWithoutNth)
          maxWeights(ni)(ci) = maxWeight
        } else {
          maxWeights(ni)(ci) = maxWeights(ni - 1)(ci)
        }
      }
    }
//    println(" After:")
//    for(ni <- 0 to n) println(maxWeights(ni).mkString(" "))

    maxWeights(n)(capacity)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val capacity: Int = s.nextInt

    val n: Int = s.nextInt
    val weights: Array[Int] = new Array[Int](n)
    var i: Int = 0
    while (i < n) {
      {
        weights(i) = s.nextInt
        i += 1
      }
    }

    val maxWeights: Array[Array[Int]] =  Array.ofDim[Int](n + 1, capacity + 1)

    println(calculateMaxWeightOfKnapsack(capacity, n, weights, maxWeights))

  }

}
