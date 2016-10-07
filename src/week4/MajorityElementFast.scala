package week4

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

/**
  * Majority rule is a decision rule that selects the alternative which has a majority,
  * that is, more than half the votes
  */
object MajorityElementFast {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

//    for (i <- 0 to 1000000) {
    val amountOfNumbers = s.nextInt()
    val numbers: ArrayBuffer[Int] = new ArrayBuffer[Int]()// Stream.continually(Random.nextInt(10)).take(amountOfNumbers).toArray//new Array[Int](amountOfNumbers)
    var i: Int = 0

    while (i < amountOfNumbers) {
      {
        numbers += s.nextInt
        i += 1
      }
    }

    def countAndCompareElements(numbers: Array[Int], candidate:Int): Int = {
        val count = numbers.count(_ == candidate)
        if(count > numbers.length / 2) candidate else -1
    }

    def getMajorityElement(numbers: Array[Int], low: Int, high: Int): Int = {
      if (high - low == 1)
        numbers(0)
      else {
        val middle: Int = (high - low) / 2
        lazy val left: Int = getMajorityElement(numbers.take(middle), 0, middle)
        lazy val right: Int = getMajorityElement(numbers.drop(middle), middle, high - low)
        if(left == right)
          right // or left
        else {
          if(left == -1) {
            countAndCompareElements(numbers, right)
          }
          else if(right == -1) {
            countAndCompareElements(numbers, left)
          }
          else {
            val leftCount = numbers.count(_ == left)
            val rightCount = numbers.count(_ == right)
            if (leftCount < rightCount)
              right
            else if (leftCount > rightCount)
              left
            else -1
          }
        }
      }
    }

    println(if(getMajorityElement(numbers.toArray, 0, amountOfNumbers) != -1) 1 else 0)

    /*val counters = mutable.Map.empty[Int, Int]
    numbers.foreach { n =>
      counters(n) = counters.get(n).map(_ + 1).getOrElse(1)
    }
    val isMajorityWithMap = counters.find { case (k, v) => v > amountOfNumbers / 2 }

    if (isMajorityWithMap.map(_ => 1).getOrElse(0) != isHasMajorityElement(numbers, amountOfNumbers, amountOfNumbers % 2, distinctNumbers, distinctNumbers.length - 1))
      println(numbers.mkString(","))*/
  }

}
