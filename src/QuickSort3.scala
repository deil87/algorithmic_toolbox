import java.util.Scanner

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import util.BenchmarkHelper._
/**
  * The goal in this problem is to redesign a given implementation of the randomized
  * quick sort algorithm so that it works fast even on sequences containing
  * many equal elements.
  */
object QuickSort3 {

  sealed trait DebugConfig {
    def enabled: Boolean = false
    def benchmark: Boolean = false
  }
  case class EnabledDebug() extends DebugConfig {
    override def enabled: Boolean = true
  }
  case class DisabledDebug() extends DebugConfig {
    override def enabled: Boolean = false
  }

  class Logger(implicit val config: DebugConfig) {
    def log(str: => String): Unit = {
      if(config.enabled) println(str) else {}
    }
  }

  implicit val debug = DisabledDebug()//DisabledDebug()
  val logger = new Logger()

  private def partition3(a: Array[Int], l: Int, r: Int): (Int, Int) = {
    logger.log(s"low: $l, high: $r")
//    for(u <- l to r) print(s"-${a(u)}-")
    val x: Int = a(r)
    logger.log(s"Pivot = $x")
    var i = 0
    var k = 0
    var p = r
    while (i < p) {
        if (a(i) < x) {
          swap(a, i, k)
          i += 1
          k +=1
        }
        else if (a(i) > x) {
          i += 1
        }
        else {
          p -= 1
          swap(a, i, p)
        }
    }

    val m = Math.min(p - k + 1, r - p + 1)
    logger.log(s"Min = $p - $k + 1 , $r - $p  = $m")

    var shift = 0
    logger.log("Before swapping : " + a.mkString(","))
    for{ t <- k to k + m - 1} {
      logger.log(s"start swapping ${k + shift}  <->  ${r - m + 1 + shift}")
        swap(a, k + shift, r - m + 1 + shift)
      shift += 1
      logger.log("end")
    }
    logger.log("After swapping : " + a.mkString(","))

    (k, p )
  }

  def swap(a: Array[Int], l: Int, j: Int): Unit = {
    if(l != j) {
      val t: Int = a(l)
      a(l) = a(j)
      a(j) = t
    }
    logger.log(a.mkString(","))
  }

  private def randomizedQuickSort(a: Array[Int], l: Int, r: Int)
  {
    if (l >= r) {
      return
    }
    val k: Int = Random.nextInt(r - l + 1) + l
    logger.log(s"Choosed random k = $k")
    swap(a, r, k)


//    val (middle1, middle2) = time("main") { partition3(a, l, r) }
//    time ("random1" ) { randomizedQuickSort(a, l, middle1 - 1) }
//    time ("random2" ) { randomizedQuickSort(a, r - middle2 + middle1 + 1, r) }
     val (middle1, middle2) =  partition3(a, l, r)
    randomizedQuickSort(a, l, middle1 - 1)
    randomizedQuickSort(a, r - middle2 + middle1 + 1, r)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val amountOfNumbers = 100000//s.nextInt()
    val numbers: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var i: Int = 0

    while (i < amountOfNumbers) {
      numbers += Random.nextInt(100000)//s.nextInt
      i += 1
    }
    val numbersArray = numbers.toArray

    time("main") { randomizedQuickSort(numbersArray, 0, amountOfNumbers - 1) }

    // Print result
    var a: Int = 0
    while (a < amountOfNumbers) {
      System.out.print(numbersArray(a) + " ")
      a += 1
    }
  }
}
