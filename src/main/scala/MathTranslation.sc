val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

def go(aList: List[String]) = ???

def sum(aList: List[Int]): Int = aList.foldLeft(0)(_ + _)

def product(aList: List[Int]): Int = aList.foldLeft(1)(_ * _)