import java.util.Scanner

/**
  * In this problem, your goal is to add parentheses to a given arithmetic expression to maximize its value.
  */

object PlacingParentheses {

  def calculateMaxValue(expression: String, minMtx: Array[Array[Int]], maxMtx: Array[Array[Int]]): Int = {

    val length1: Int = expression.length
    val amountOfNumbers: Int = length1 / 2 + 1
    for(i <- 0 until amountOfNumbers) {
      val expression1: Char = expression(i * 2)
      minMtx(i)(i) = expression1.asDigit
      maxMtx(i)(i) = expression(i * 2).asDigit
    }

//    println(" Before Min:")
//    for(ni <- 0 until amountOfNumbers) println(minMtx(ni).mkString(" "))
//    println(" Before Max:")
//    for(ni <- 0 until amountOfNumbers) println(maxMtx(ni).mkString(" "))

    for( s <- 1 until amountOfNumbers) {
      for (i <- 0 until amountOfNumbers - s) {
        val j =  i + s
        val (min, max) = minAndMax(i, j, expression, minMtx, maxMtx)
        minMtx(i)(j) = min
        maxMtx(i)(j) = max
      }
    }

//    println(" After Min:")
//    for(ni <- 0 until amountOfNumbers) println(minMtx(ni).mkString(" "))
//    println(" After Max:")
//    for(ni <- 0 until amountOfNumbers) println(maxMtx(ni).mkString(" "))

    maxMtx(0)(amountOfNumbers - 1)
  }

  def minAndMax(i: Int, j: Int, expression: String, minMtx: Array[Array[Int]], maxMtx: Array[Array[Int]]): (Int, Int) = {
    var min = Int.MaxValue
    var max = Int.MinValue
    for (k <- i until j) {
      val lastOperationChar: Char = expression(2* (k + 1) - 1)
      lazy val a = op(lastOperationChar)(maxMtx(i)(k) , maxMtx(k + 1)(j) )
      lazy val b = op(lastOperationChar)(maxMtx(i)(k) , minMtx(k + 1)(j) )
      lazy val c = op(lastOperationChar)(minMtx(i)(k) , maxMtx(k + 1)(j) )
      lazy val d = op(lastOperationChar)(minMtx(i)(k) , minMtx(k + 1)(j) )

      val variants = List(a, b, c, d)
      val maximum = (max :: variants).sortWith(_ > _).head
      val minimum = (min :: variants).sortWith(_ < _).head
      if( maximum > max) max = maximum
      if( minimum < min) min = minimum
    }
    (min, max)
  }

  def op(strOp: Char)(l: Int, r: Int) = strOp match {
    case '+' =>  l + r
    case '-' =>  l - r
    case '*' =>  l * r
  }


  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val str1 = s.next()


    val minMtx: Array[Array[Int]] =  Array.ofDim[Int]( str1.length / 2 + 1, str1.length / 2 + 1)
    val maxMtx: Array[Array[Int]] =  Array.ofDim[Int]( str1.length / 2 + 1, str1.length / 2 + 1)

    println(calculateMaxValue(str1, minMtx, maxMtx))

  }

}
