package org.coursera.alg_toolbox.week4

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  *
  */
object QuickSort {
  private def partition2(a: Array[Int], l: Int, r: Int): Int = {
    val x: Int = a(l)
    var j: Int = l
    var i: Int = l + 1
    while (i <= r) {
        if (a(i) <= x) {
          j += 1
          val t: Int = a(i)
          a(i) = a(j)
          a(j) = t
        }
      i+=1
    }
    val t: Int = a(l)
    a(l) = a(j)
    a(j) = t
    j
  }

  private def randomizedQuickSort(a: Array[Int], l: Int, r: Int)
  {
    if (l >= r) {
      return
    }
    val k: Int = Random.nextInt(r - l + 1) + l
    val t: Int = a(l)
    a(l) = a(k)
    a(k) = t
    val m: Int = partition2(a, l, r)
    randomizedQuickSort(a, l, m - 1)
    randomizedQuickSort(a, m + 1, r)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val amountOfNumbers = s.nextInt()
    val numbers: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var i: Int = 0

    while (i < amountOfNumbers) {
      numbers += s.nextInt
      i += 1
    }
    val numbersArray = numbers.toArray

    randomizedQuickSort(numbersArray, 0, amountOfNumbers - 1)

    // Print result
    var a: Int = 0
    while (a < amountOfNumbers) {
      System.out.print(numbersArray(a) + " ")
      a += 1
    }
  }
}
