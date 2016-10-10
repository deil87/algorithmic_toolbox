package week4

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

/**
  *  How Close a Data is To Being Sorted?
  */
object InversionsOrMergeSort {

  private def mergeSort(array: Array[Int], l: Int, r: Int , inversionsCount: BigInt): (Array[Int], BigInt) = {
    if (array.length == 1) { // one element left in array
        (array, 0)
    } else {
      val halfIndex = if(r % 2 == 1) r / 2 + 1 else r / 2
      val leftSubArray = array.slice(0, halfIndex)
      val rightSubArray = array.slice(halfIndex, r + 1)
      val (arrLeft, countLeft) = mergeSort(leftSubArray, 0, halfIndex -1, inversionsCount)
      val (arrRight, countRight) = mergeSort(rightSubArray, 0, r - halfIndex, inversionsCount)

      val (joined, joinedCount) = join(halfIndex, arrLeft, arrRight)
      (joined, inversionsCount + countLeft + countRight + joinedCount)
      }
  }

  def join(halfIndex: Int, arrLeft: Array[Int], arrRight: Array[Int]): (Array[Int], BigInt) = {
    var i = 0
    var j = 0
    var inversionsCount: BigInt = 0
    val joinedBuffer = new ArrayBuffer[Int]()
    val leftLength: Int = arrLeft.length
    val rightLength: Int = arrRight.length
    while (i < leftLength || j < rightLength) {
      if(i == leftLength) {
        joinedBuffer += arrRight(j)
        j += 1
      }
      else if(j == rightLength) {
        joinedBuffer += arrLeft(i)
        i += 1
      } else {
        if (arrLeft(i) < arrRight(j)) {
          joinedBuffer += arrLeft(i)
          i += 1
        }
        else if (arrLeft(i) > arrRight(j)) {
          joinedBuffer += arrRight(j)
          j += 1
          inversionsCount += 1 * (leftLength - i)
        }
        else {
          joinedBuffer += arrLeft(i)
          i += 1
        }
      }
    }

    val joinedArray: Array[Int] = joinedBuffer.toArray
    (joinedArray, inversionsCount)
  }

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

      val amountOfNumbers = s.nextInt
      val numbers: ArrayBuffer[Int] = new ArrayBuffer[Int]()
      var i: Int = 0

      while (i < amountOfNumbers) {
        numbers += s.nextInt//Random.nextInt(10) //amountOfNumbers - i//s.nextInt
        i += 1
      }
      val numbersArray = numbers.toArray//.sortWith(_ > _)

      val (sorted, inversions) = mergeSort(numbersArray, 0, amountOfNumbers - 1, 0)

      println(inversions)

  }


  def naiveMethod(array: Array[Int]): BigInt = {
    val length = array.length
    var inversions: BigInt = 0
    for (i <- 0 until length) {
      for (j <- i until length) {
        if (array(i) > array(j))
          inversions += 1
      }
    }
    inversions
  }
}
