package week4

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.StringTokenizer


/**
  * Created by deil on 03/10/16.
  */
object BinarySearch {

  def binarySearch(sortedSource: Array[Int], low: Int, high: Int, target: Int): Int = {
    val middleIndex = low + ((high - low) / 2)
    if( low > high)
      -1
    else {
      val middleElement: Int = sortedSource(middleIndex)
      if (middleElement > target) {
//        println("middleElement > target")
        binarySearch(sortedSource, low, middleIndex - 1, target)
      }
      else if (middleElement < target) {
//        println("middleElement < target")
        binarySearch(sortedSource, middleIndex + 1, high, target)
      }
      else {
//        println("middleElement == target")
        middleIndex
      }

    }
  }

  def main(args: Array[String]) {
    val scanner: BinarySearch.FastScanner = new BinarySearch.FastScanner(System.in)
    val n: Int = scanner.nextInt
    val a: Array[Int] = new Array[Int](n)
    for (i <- 0 until n) {
        a(i) = scanner.nextInt
    }
    val sortedSourceArray = a.sortWith(_ < _)
    val m: Int = scanner.nextInt
    val b: Array[Int] = new Array[Int](m)
    for (i <- 0 until m) {
        b(i) = scanner.nextInt
    }
    for (i <- 0 until m) {
        print(binarySearch(sortedSourceArray, 0, n - 1, b(i)) + " ")
    }
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
