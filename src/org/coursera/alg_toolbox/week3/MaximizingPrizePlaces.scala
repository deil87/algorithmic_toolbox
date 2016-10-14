package org.coursera.alg_toolbox.week3

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

/**
  * You are organizing a funny competition for children. As a prize fund you have푛candies.
  * You would like to use these candies for top푘places in a competitionwith a natural restriction that a higher
  * place gets a larger number of candies.To make as many children happy as possible,
  * you are going to find the largestvalue of푘for which it is possible
  */
object MaximizingPrizePlaces {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val amountOfCandles = s.nextInt()


    def calcMaxPrizesPlacesFor(amountOfCandles: Int, currentPrizeSize: Int,  prizes: ArrayBuffer[Int]): ArrayBuffer[Int] = amountOfCandles match {
      case v if v == 0 => prizes
      case v if v > 0 =>
        if(  amountOfCandles > 2 * currentPrizeSize) {
          prizes += currentPrizeSize
          calcMaxPrizesPlacesFor(amountOfCandles - currentPrizeSize, currentPrizeSize + 1, prizes)
        }
        else {
          prizes += amountOfCandles
          prizes
        }
    }

    val prizes: ArrayBuffer[Int] = calcMaxPrizesPlacesFor(amountOfCandles, 1, ArrayBuffer.empty[Int])
    println(prizes.length)
    println(prizes.mkString(" "))
  }


}
