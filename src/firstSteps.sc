(0,5)
25 % 4

def first(num:Int): Int = if(num % 10 == num) num else first(num/10)

first(10145)
first(8934)

def sumPar(l:List[Int]): List[Int] = if(l.size < 2) List[Int]() else l(0)+l(1) :: sumPar(l.tail)

sumPar(List(1,2,3,4))

def tartlev(n:Int):List[Int] =
  if(n == 1)
    List(1)
  else
    sumPar(tartlev(n-1)).prepended(1).appended(1)

tartlev(7)

def tart(n:Int):List[List[Int]] =
  if(n == 1)
    List(tartlev(1))
  else
    tart(n-1).appended(tartlev(n))

tart(5)