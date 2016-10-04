import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/**
  * Majority rule is a decision rule that selects the alternative which has a majority,
  * that is, more than half the votes
  */
object MajorityElement {
  def main(args: Array[String]): Unit = {

    //    val s = new Scanner(System.in)

    for (i <- 0 to 10) {
      val amountOfNumbers = 100000 //s.nextInt()
      val numbers: Array[Int] = Stream.continually(Random.nextInt(1000000000)).take(amountOfNumbers).toArray //new Array[Int](amountOfNumbers)
      var i: Int = 0

      /*while (i < amountOfNumbers) {
      {
        numbers(i) = s.nextInt
        i += 1
      }
    }*/

      def isMajority(numbers: Array[Int], element: Int, from: Int, to: Int): Int = {
        var count = 0
        var a = from
        while (a < to) {
          if (numbers(a) == element) count += 1
          a += 1
        }
        count
      }

      @tailrec def isHasMajorityElement(numbers: Array[Int], numbersSize: Int, distinct: Array[Int], distSize: Int, current: Int): Int = {
        if (current > distSize)
          0
        else {
          val middle: Int = numbersSize / 2
          lazy val left: Int = isMajority(numbers, distinct(current), 0, middle)
          lazy val right: Int = isMajority(numbers, distinct(current), numbersSize - middle - {
            if (numbersSize % 2 == 1) 1 else 0
          }, numbersSize)
          val isCurrentElementMajority =
            if (right > middle)
              true
            else if (left > middle)
              true
            else if (left + right > middle)
              true
            else false
          isCurrentElementMajority match {
            case true => 1
            case _ => isHasMajorityElement(numbers, numbersSize, distinct, distSize, current + 1)
          }
        }

      }

          def distinctNumbers: Array[Int] = numbers.distinct
      //    println(isHasMajorityElement(numbers, amountOfNumbers, distinctNumbers, distinctNumbers.length - 1, 0))

      val counters = mutable.Map.empty[Int, Int]
      numbers.foreach { n =>
        counters(n) = counters.get(n).map(_ + 1).getOrElse(1)
      }
      val isMajorityWithMap = counters.find { case (k, v) => v > amountOfNumbers / 2 }

      if(isMajorityWithMap.map(_ => 1).getOrElse(0) != isHasMajorityElement(numbers, amountOfNumbers, distinctNumbers, distinctNumbers.length - 1, 0))
        println(numbers.mkString(","))

    }
  }


}
