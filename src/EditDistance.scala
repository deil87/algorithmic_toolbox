import java.util.Scanner

/**
  * The edit distance between two strings is the minimum number of insertions, deletions, and mismatches in
an alignment of two strings
  */

object EditDistance {

  def calculateEditDistance(str1: String, str2: String, distancesMtx: Array[Array[Int]]): Int = {

    val length1: Int = str1.length
    val length2: Int = str2.length
    for(i <- 0 to length1) distancesMtx(i)(0) = i
    for(j <- 0 to length2) distancesMtx(0)(j) = j

//    println(" Before:")
//    for(ni <- 0 to length1) println(distancesMtx(ni).mkString(" "))

    for(i <- 1 to length1) {
      for(j <- 1 to length2) {
        lazy val insertion = distancesMtx(i)(j - 1) + 1
        lazy val deletion = distancesMtx(i - 1)(j) + 1
        lazy val matched = distancesMtx(i -1 )(j - 1)
        lazy val mismatched = distancesMtx(i - 1)(j - 1) + 1

        val variants = List(insertion, deletion)
        val minimal =
          if(str1(i -1) == str2(j -1))
            (matched :: variants).sortWith(_ < _).head
        else
            (mismatched :: variants).sortWith(_ < _).head

        distancesMtx(i)(j) = minimal

      }
    }
//    println(" After:")
//       for(ni <- 0 to length1) println(distancesMtx(ni).mkString(" "))
    distancesMtx(length1)(length2)
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val str1 = s.next()
    val str2 = s.next()


    val maxWeights: Array[Array[Int]] =  Array.ofDim[Int]( str1.length + 1, str2.length + 1)

    println(calculateEditDistance(str1, str2, maxWeights))

  }

}
