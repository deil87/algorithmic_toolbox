val str1 =  "21"
val str2 =  "215"

def isGreaterOrEqual(digit: String, maxDigit: String): Boolean = {
  if(maxDigit.indexOf(digit) == 0)
    digit(0) >= maxDigit(digit.length)
  else {
    println("hello")
    digit > maxDigit
  }
}

val res = isGreaterOrEqual(str1, str2)

val tmp = "2" < "21"


val l = List(6, 1,3,6)
l.span(n => n != 6)

//Seq(1,2,3,4).span(x => x % 2 == 0)
//
//List(1, 2, 3, -4, 5) span (_ > 0)
