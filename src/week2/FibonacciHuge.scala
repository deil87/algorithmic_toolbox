package week2

import java.util.Scanner

import scala.collection.mutable

/**
  * In this problem, your goal is to compute Fn modulo m, where n may be really huge: up to 10^18.
  **/
object FibonacciHuge {

  def main(args: Array[String]): Unit = {

    val cache =  mutable.ArrayBuffer.empty[Int]
    cache += 0
    cache += 1

    val s = new Scanner(System.in)
    val fibNumberConsole = s.nextBigInteger()
    val moduloConsole = s.nextInt()
    var flag = true

    def calc_fibModulo(fibNumber: BigInt, modulo: Int): BigInt = {
      try {
        for (number <- 2 to Int.MaxValue) {
          if (cache(number - 2) == 0 && cache(number - 1) == 1 && number != 2) {
            throw new IllegalArgumentException("")
          } else {
            val first = cache(number - 1)
            val second = cache(number - 2)
            val value = (first + second) % modulo
            cache += value
//            println(s"$number -> $value")
          }

        }
        throw new IllegalStateException("Failed to figure out Pisano period")
      } catch {
        case ex: IllegalArgumentException =>
          val pisanoPeriod = cache.size - 2
//          println("Pisano period:" + pisanoPeriod)
          val reminder = fibNumber % pisanoPeriod toInt

          cache(reminder)
      }
    }

    println {calc_fibModulo(fibNumberConsole, moduloConsole)}
  }
}
