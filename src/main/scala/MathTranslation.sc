val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

def go(stringList: List[String]) = {
  val intList: List[Int] = convert(stringList)
  println("Translation: " + intList.mkString(" "))
  println("Addition: " + intList.mkString(" + ") + " = " + sum(intList))
  println("Multiplication: " + intList.mkString(" * ") + " = " + product(intList))
}

def convert(aList: List[String]): List[Int] =
  aList match{
    case Nil => Nil
    case head::tail if member(head, chinese) => chinese.indexOf(head)::convert(tail)
    case head::tail if member(head, english) => english.indexOf(head)::convert(tail)
    case head::tail => convert(tail)
  }

def sum(aList: List[Int]): Int = aList.foldLeft(0)(_ + _)

def product(aList: List[Int]): Int = aList.foldLeft(1)(_ * _)

def member(n: String, myList: List[String]): Boolean = {
  myList match{
    case Nil => false
    case listHead :: listTail => if(n == listHead) true else member(n, listTail)
  }
}

go(List("yi", "nine", "six", "ba"))
go(List("yi", "josh", "three", "si"))
