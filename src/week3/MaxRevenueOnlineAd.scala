package week3

import java.util.Scanner

/**
  * You have푛ads to place on a popular Internet page.
  * For each ad, you know howmuch is the advertiser willing to pay for one click on this ad.
  * You have set up푛slots on your page and estimated the expected number of clicks per day for eachslot.
  * Now, your goal is to distribute the ads among the slots to maximize thetotal revenue.
  */
object MaxRevenueOnlineAd {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val n: Int = s.nextInt
    val profits: Array[Int] = new Array[Int](n)
    val numberOfClicks: Array[Int] = new Array[Int](n)
    var i: Int = 0
    while (i < n) {
      {
        profits(i) = s.nextInt()
        i += 1
      }
    }
    i = 0
    while (i < n) {
      {
        numberOfClicks(i) = s.nextInt
        i += 1
      }
    }

    def calcMaxRevenueFromAd( profits: Array[Int], clicks: Array[Int]): BigInt = {
      profits.zip(clicks).map{ case (v, w) => BigInt(v) * BigInt(w) }.sum
    }


    println(calcMaxRevenueFromAd( profits.sortWith(_>_), numberOfClicks.sortWith(_>_)))
  }


}
