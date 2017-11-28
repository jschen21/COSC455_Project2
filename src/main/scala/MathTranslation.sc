val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

def go(stringList: List[String]) = {
  val intList: List[Int] = convert(stringList)
  println("Translation: " + intList.mkString(" "))
  println("Addition: " + intList.mkString(" + ") + " = " + sum(intList))
  println("Multiplication: " + intList.mkString(" * ") + " = " + product(intList))
}

def convert(aList: List[String]): List[Int] = ???

def sum(aList: List[Int]): Int = aList.foldLeft(0)(_ + _)

def product(aList: List[Int]): Int = aList.foldLeft(1)(_ * _)