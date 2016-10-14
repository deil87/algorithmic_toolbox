package org.coursera.alg_toolbox.week2

import java.util.Scanner

import scala.collection.mutable

/**
  * Created by deil on 01/10/16.
  */
object FibonacciLastDigit {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val a = s.nextInt()



    def calc_fibModulo10(fibNumber: Int, modulo: Int): Int = {
      val cache = mutable.ArrayBuffer.empty[Int]
      cache += 0
      cache += 1

      for (number <- 2 to fibNumber) {
        val lastTwoElements = cache.takeRight(2)
        val first: Int = lastTwoElements.head
        val second: Int = lastTwoElements.last
        val value: Int = (first + second) % modulo
        cache += value
      }
      cache(fibNumber)
    }


    println(calc_fibModulo10(a, 10))
  }


}
