package week3

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

/**
  * You are responsible for collecting signatures from all tenants of a certain build-ing.
  * For each tenant, you know a period of time when he or she is at home.
  * You would like to collect all signatures by visiting the building as few times aspossible.
  * The mathematical model for this problem is the following.
  * You are given aset of segments on a line and your goal is to mark as few points on a line aspossible so that
  * each segment contains at least one marked point
  */
object CollectingSignatures {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val n: Int = s.nextInt
    val begins: Array[Int] = new Array[Int](n)
    val ends: Array[Int] = new Array[Int](n)
    var i: Int = 0
    while (i < n) {
      {
        begins(i) = s.nextInt()
        ends(i) = s.nextInt
        i += 1
      }
    }


    def calcMaxValueOfLoot( begins: Array[Int], ends: Array[Int]): ArrayBuffer[Int] = {
      val orderedByEndTimeOfPresence = begins.zip(ends).sortWith(_._2 < _._2)

      def calcHelper( orderedTenantsTimes: Array[(Int, Int)], points: ArrayBuffer[Int]): ArrayBuffer[Int] = orderedTenantsTimes match  {
        case v if orderedTenantsTimes.isEmpty => points
        case tenantTimes =>
          val nextVisit = tenantTimes.head
          val unvisitedTenantsTimes = tenantTimes.dropWhile(_._1 <= nextVisit._2)
          points += nextVisit._2
          calcHelper(unvisitedTenantsTimes, points)
      }

      calcHelper( orderedByEndTimeOfPresence, ArrayBuffer.empty[Int])
    }


    val points: ArrayBuffer[Int] = calcMaxValueOfLoot(begins, ends)
    println(points.length)
    println(points.mkString(" "))
  }


}
