package org.coursera.data_structures.week2

import java.util.Scanner

import scala.collection.mutable

/**
  * In this problem you will implement a feature for a text editor to find errors in the usage of brackets in the
  * code
  */
object CheckBracketsInCode {

  val setOfOpeningBrackets = List(('[',1),('{',2),('(',3))
  val setOfClosingBrackets = List((']',1),('}',2),(')',3))
  private def checkForErrors(code: String): Either[String,Int] = {
    val stack = new mutable.Stack[(Char, Int, Int)]() // (char, type, index in code string)
    var firstUnmatchedClosing = -1
    var i = 0
    val lengthOfCode: Int = code.length
    while(i < lengthOfCode) {
      val isOpeningChar = setOfOpeningBrackets.find { case (c, t) => c == code(i) }
      isOpeningChar.foreach{ elem =>
        stack.push((elem._1, elem._2, i))
      }
      if(isOpeningChar.isEmpty) {
        setOfClosingBrackets.find { case (c, t) => c == code(i) }.foreach { elem =>
          val popped = if(stack.isEmpty) None else Some(stack.pop())
          popped match {
            case Some(lastOpenedBracket) =>
              if (elem._2 != lastOpenedBracket._2) {
                firstUnmatchedClosing = i + 1//lastOpenedBracket._3 + 1
                i = lengthOfCode
              }
            case None => {
              firstUnmatchedClosing = i + 1
              i = lengthOfCode
            }
          }
        }
      }
      i+=1
    }
    if(firstUnmatchedClosing < 0 && stack.isEmpty)
      Left("Success")
    else {
      if(firstUnmatchedClosing >= 0)
        Right(firstUnmatchedClosing)
      else{
          var lastOpenedUnmatched = -1
          var i = lengthOfCode - 1
          while (i > -1) {
            setOfOpeningBrackets.find { case (c, t) => c == code(i) }.foreach { _ =>
              lastOpenedUnmatched = i + 1
              i = -1
            }
            i -= 1
          }
        Right(stack.pop()._3 + 1)
      }
    }

  }

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

      val codeString = s.next()


      val result = checkForErrors(codeString)

      result.fold(println(_), println(_))
  }

}
