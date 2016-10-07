package week4

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Majority rule is a decision rule that selects the alternative which has a majority,
  * that is, more than half the votes
  */
object MajorityElement {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

//    for (i <- 0 to 1000000) {
    val amountOfNumbers = s.nextInt()
    val numbers: ListBuffer[Int] = new ListBuffer[Int]()// Stream.continually(Random.nextInt(10)).take(amountOfNumbers).toArray//new Array[Int](amountOfNumbers)
    var i: Int = 0

    while (i < amountOfNumbers) {
      {
        numbers += s.nextInt
        i += 1
      }
    }

    def isMajority(numbers: List[Int], element: Int, from: Int, to: Int): Int = {
      var count = 0
      var a = from
      while (a < to) {
        if (numbers(a) == element) count += 1
        a += 1
      }
      count
    }

    @tailrec def isHasMajorityElement(numbers: List[Int], numbersSize: Int, isOdd: Int, distinct: List[Int], distSize: Int): Int = {
      if (distSize <= 0)
        0
      else {
        val middle: Int = numbersSize / 2
        lazy val currentDistinct: Int = distinct.head
        lazy val left: Int = isMajority(numbers, currentDistinct, 0, middle)
        lazy val right: Int = isMajority(numbers, currentDistinct, numbersSize - middle - isOdd, numbersSize)
        val isCurrentElementMajority =
          if (right > middle || left > middle || left + right > middle)
            true
          else false

        if (isCurrentElementMajority)
          1
        else
          isHasMajorityElement(numbers, numbersSize, isOdd, distinct.drop(1), distSize - 1)
      }

    }

    val allNumbers = numbers.toList
    val distinctNumbers = allNumbers.distinct
    println(isHasMajorityElement(allNumbers, amountOfNumbers, amountOfNumbers % 2, distinctNumbers, distinctNumbers.length))

    /*val counters = mutable.Map.empty[Int, Int]
    numbers.foreach { n =>
      counters(n) = counters.get(n).map(_ + 1).getOrElse(1)
    }
    val isMajorityWithMap = counters.find { case (k, v) => v > amountOfNumbers / 2 }

    if (isMajorityWithMap.map(_ => 1).getOrElse(0) != isHasMajorityElement(numbers, amountOfNumbers, amountOfNumbers % 2, distinctNumbers, distinctNumbers.length - 1))
      println(numbers.mkString(","))*/
  }

}
