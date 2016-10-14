package org.coursera.alg_toolbox.week4

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.StringTokenizer

import scala.util.Random

/**
  * The goal in this problem is to redesign a given implementation of the randomized
  * quick sort algorithm so that it works fast even on sequences containing
  * many equal elements.
  */
object QuickSort3DutchNationalFlagMethod { // "Dutch National Flag” problem - too much overhead. To many unnecessary swaps

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
//    logger.log(s"low: $l, high: $r")
//    for(u <- l to r) print(s"-${a(u)}-")
    val x: Int = a(r)
//    logger.log(s"Pivot = $x")
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
//    logger.log(s"Min = $p - $k + 1 , $r - $p  = $m")

    var shift = 0
//    logger.log("Before swapping : " + a.mkString(","))
    for{ t <- k to k + m - 1} {
//      logger.log(s"start swapping ${k + shift}  <->  ${r - m + 1 + shift}")
        swap(a, k + shift, r - m + 1 + shift)
      shift += 1
//      logger.log("end")
    }
//    logger.log("After swapping : " + a.mkString(","))

    (k, p )
  }
  var counter: Long = 0L

  def swap(a: Array[Int], l: Int, j: Int): Unit = {
    if(l != j) {
      val t: Int = a(l)
      a(l) = a(j)
      a(j) = t
    } else { counter += 1}
//    logger.log(a.mkString(","))
  }

  private def randomizedQuickSort(a: Array[Int], l: Int, r: Int)
  {
    if (l >= r) {
      return
    }
    val k: Int = Random.nextInt(r - l + 1) + l
//    logger.log(s"Choosed random k = $k")
    swap(a, r, k)

     val (middle1, middle2) =  partition3(a, l, r)
    randomizedQuickSort(a, l, middle1 - 1)
    randomizedQuickSort(a, r - middle2 + middle1 + 1, r)
  }


  def main(args: Array[String]): Unit = {

    val s = new FastScanner(System.in)

    val amountOfNumbers = 100000//s.nextInt
    val numbers: Array[Int] = new Array[Int](amountOfNumbers)
    var i: Int = 0

    while (i < amountOfNumbers) {
      numbers(i) = Random.nextInt(1000000000)//s.nextInt
      i += 1
    }


    randomizedQuickSort(numbers, 0, amountOfNumbers - 1)

    // Print result
    var a: Int = 0
    while (a < amountOfNumbers) {
      System.out.print(numbers(a) + " ")
      a += 1
    }
    println("\n Counter:" + counter)
  }

  class FastScanner(stream: InputStream) {
    private var br: BufferedReader = new BufferedReader(new InputStreamReader(stream))
    private var st: StringTokenizer = null

    def next: String = {
      while (st == null || !st.hasMoreTokens) {
        {
          try {
            st = new StringTokenizer(br.readLine)
          }
          catch {
            case e: IOException => {
              e.printStackTrace
            }
          }
        }
      }
      return st.nextToken
    }

    def nextInt: Int = {
      return next.toInt
    }
  }
}
