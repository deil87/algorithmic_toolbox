import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A thief finds much more loot than his bag can fit.
  * Help him to find the most valuable combi-nation of items assuming that any fraction of a loot item
  * can be put into his bag
  */
object FractionalKnapsack {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val n: Int = s.nextInt
    val capacity: Int = s.nextInt
    val values: Array[Double] = new Array[Double](n)
    val weights: Array[Int] = new Array[Int](n)
    var i: Int = 0
    while (i < n) {
      {
        values(i) = s.nextDouble()
        weights(i) = s.nextInt
        i += 1
      }
    }


    def calcMaxValueOfLoot(capacity: Int, values: Array[Double], weights: Array[Int]): Double = {
      val orderedByValuePerCapacityUnit = values.zip(weights).map{ case (v, w) => (v , w, v.toDouble / w) }.sortWith(_._3 > _._3)

      def calcHelper(capacity: Int, orderedItemsInfo: Array[(Double, Int, Double)], total: Double): Double = capacity match  {
        case v if v == 0 | orderedItemsInfo.isEmpty => total
        case v if v > 0 =>
          val (value, weight, ratio) = orderedItemsInfo.head
          if( weight <= capacity)
            calcHelper( capacity - weight, orderedItemsInfo.drop(1), total + weight * ratio)
          else {
            val onlyPart = capacity
            orderedItemsInfo(0) = (value - capacity * ratio, weight - capacity, ratio)
            calcHelper(0,  orderedItemsInfo, total + onlyPart * ratio)
          }
      }

      calcHelper(capacity, orderedByValuePerCapacityUnit, 0)
    }


    println(calcMaxValueOfLoot(capacity, values, weights))
  }


}
