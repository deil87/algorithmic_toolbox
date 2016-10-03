package week3

import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * In this problem, you will design and implement an elementary greedy
  * algorithmused by cashiers all over the world millions of times per day.
  */
object ChangingMoney {
  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val inputValue = s.nextInt()

    val typesOfCoins = mutable.ArrayBuffer(1,10,5)

    val sortedTypesOfCoins = typesOfCoins.sorted(Ordering[Int].reverse)


    def calcMinAmountOfCoins(value: Int, typeOfCoins: ArrayBuffer[Int], amountOfCoins: Int): Int = value match {
      case v if v == 0 => amountOfCoins
      case v if v > 0 =>
        val nextMostValueCoinToConsider: Int = typeOfCoins.head
        val howManyCoinsFitsValue: Int = value / nextMostValueCoinToConsider
        if( howManyCoinsFitsValue > 0)
          calcMinAmountOfCoins( value - howManyCoinsFitsValue * nextMostValueCoinToConsider,
            typeOfCoins.drop(1), amountOfCoins + howManyCoinsFitsValue)
        else {
          calcMinAmountOfCoins(value, typeOfCoins.drop(1), amountOfCoins)
        }
    }

    println(calcMinAmountOfCoins(inputValue, sortedTypesOfCoins, 0))
  }


}
