def prime (i : Int): Boolean =
  i < 2 match {
    case true => false
    case false => !((2 until i - 1) exists (i % _ == 0))
  }

prime(1)
prime(2)
prime(7)
prime(10)

def twinprimes(i : Int, j : Int): Boolean =
  Math.abs(i - j) match {
    case 2 =>
      if(prime(i) && prime(j)) true
      else false
    case _ => false
  }

twinprimes(41, 43)
twinprimes(43, 47)
twinprimes(3, 5)
twinprimes(1, 3)

def twinprimeslist(n : Int): List[Int] = duplicateChecker(twinprimeslisthelper(n).reverse)

def twinprimeslisthelper(n : Int): List[Int] =
  n match{
    case 3 => Nil
    case _ =>
      if(twinprimes(n, n-2)) n::n-2::twinprimeslisthelper(n-2)
      else twinprimeslisthelper(n-1)
  }

def duplicateChecker(aList: List[Int]): List[Int] =
  aList match{
    case Nil => Nil
    case head::List() => List(head)
    case head::tail if head == tail.head => duplicateChecker(tail)
    case head::tail => head::duplicateChecker(tail)
  }

twinprimeslist(50)
twinprimeslist(100)

def goldbach(i : Int) =
  i match{
    case i if i <= 2 => println(i + " must be greater than 2")
    case (i) if i % 2 == 1 => println(i + " must be an even number")
    case (i) if i % 2 == 0 =>
      var j = 1
      goldbachhelper(i, j)
  }

def goldbachhelper(i : Int, j : Int): Unit = {
  var num1 = i
  var num2 = j
  j match{
    case j if(prime(i - j) && prime(j)) => println(j + " + " + (i - j) + " = " + i)
    case _ => goldbachhelper(num1, num2+1)
  }
}

goldbach(2)
goldbach(3)
goldbach(4)
goldbach(28)