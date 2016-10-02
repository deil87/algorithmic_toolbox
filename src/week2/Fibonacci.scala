package week2

import java.util.Scanner

import scala.collection.mutable

/**
  * Created by deil on 01/10/16.
  */
object Fibonacci {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val a = s.nextInt()

    val cache = mutable.Map.empty[Int, Int]
    cache.update(0, 0)
    cache.update(1, 1)

    def calc_fib(n: Int): Int = {
      n match {
        case number if number <= 1 => number
        case number if number > 1 => cache.getOrElseUpdate(number - 1, calc_fib(number - 1)) +
          cache.getOrElseUpdate(number - 2, calc_fib(number - 2))
      }
    }

    println(calc_fib(a))
  }


}
