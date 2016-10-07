package util

/**
  * Created by deil on 30/09/16.
  */
object BenchmarkHelper {

  def time[R](marker: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"$marker# Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}
