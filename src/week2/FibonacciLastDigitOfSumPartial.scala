package week2

import java.util.Scanner

import scala.collection.mutable

/**
  * The goal in this problem is to find the last digit of a sum of the first n Fibonacci numbers.
  */
object FibonacciLastDigitOfSumPartial {

  def main(args: Array[String]): Unit = {

    val cache = mutable.ArrayBuffer.empty[Int]
    cache += 0
    cache += 1

    val s = new Scanner(System.in)
    val fibNumberFrom = s.nextBigInteger()
    val fibNumberTo = s.nextBigInteger()

    def calc_lastDigitOfSumPartial(fibNumberFrom: BigInt, fibNumberTo: BigInt, modulo: Int): Int = {
      try {
        for (number <- 2 to Int.MaxValue) {
          val lastTwoElements = cache.takeRight(2)
          if (lastTwoElements.head == 0 && lastTwoElements.last == 1 && number != 2) {
            throw new IllegalArgumentException("")
          } else {
            val first: Int = cache(number - 1)
            val second: Int = cache(number - 2)
            val value: Int = (first + second) % modulo
            cache += value

          }

        }
        throw new IllegalStateException("Failed to figure out Pisano period")
      } catch {
        case ex: IllegalArgumentException =>
          //cache.zipWithIndex.foreach{ case (value, number) => println(s"$number -> $value")}
          val pisanoPeriod = cache.size - 2
          val sumOfPeriod = cache.sum - 1
//          println("Pisano period:" + pisanoPeriod)
//          println("Sum of period:" + sumOfPeriod)

          val repeatNumberFrom = fibNumberFrom / pisanoPeriod
          val repeatNumberTo = fibNumberTo / pisanoPeriod
//          println("repeatNumberFrom :" + repeatNumberFrom)
//          println("repeatNumberTo :" + repeatNumberTo)

          val reminderFrom = fibNumberFrom % pisanoPeriod toInt // Assuming that there is no lose of precision
          val reminderTo = fibNumberTo % pisanoPeriod toInt // Assuming that there is no lose of precision
          //println("reminder :" + reminder)


          val repeatedSumFrom = sumOfPeriod * repeatNumberFrom
          val repeatedSumTo = sumOfPeriod * repeatNumberTo
//          println("repeatedSumFrom :" + repeatedSumFrom)
//          println("repeatedSumTo :" + repeatedSumTo)

          val reminderSumFrom = cache.take(reminderFrom ).sum
          val reminderSumTo = cache.take(reminderTo + 1).sum
//          println(s"reminderSumFrom (${cache.mkString(",")}:" + reminderSumFrom)
//          println(s"reminderSumTo (${cache.mkString(",")}:" + reminderSumTo)

          (repeatedSumTo + BigInt(reminderSumTo) - repeatedSumFrom - BigInt(reminderSumFrom) ) % modulo toInt // Assuming that there is no lose of precision
      }
    }

    println {calc_lastDigitOfSumPartial(fibNumberFrom, fibNumberTo, 10)}
  }
}
