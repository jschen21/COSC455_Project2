def prime (i : Int) : Boolean = {
  i < 2 match {
    case true => false
    case false => !((2 until i - 1) exists (i % _ == 0))
  }
}

prime(1)
prime(2)
prime(7)
prime(10)

def twinprimes(i : Int, j : Int) : Boolean = {
  Math.abs(i - j) match {
    case 2 =>
      if(prime(i) && prime(j)) true
      else false
    case _ => false
  }
}

twinprimes(41, 43)
twinprimes(43, 47)
twinprimes(1, 3)

def twinprimeslist(n : Int) : List[Int] = ???


def goldbach(i : Int) = ???