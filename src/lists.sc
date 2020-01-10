import com.sun.xml.internal.fastinfoset.algorithm.BooleanEncodingAlgorithm

def member[A](list:List[A], memb:A):Boolean =
  list match {
    case Nil => false
    case head :: tail => head == memb || member(tail, memb)
  }

member(List(1, 2, 3, 4, 5), 5)
member(List(1, 3, 3, 4, 5), 6)
member(List('a', 'd'), 'c')
member(List(true, true), true)
member(Nil, 3)
member(List(1), 6)
member(List(1), 1)

def middlePoint(interval:(Double, Double)): Double =
  interval match {
    case (min, max) => (min+max)/2
  }

def middlePoints(list: List[(Double, Double)]): List[Double] =
  list match {
    case Nil => Nil
    case head :: tail => middlePoint(head) :: middlePoints(tail)
  }

middlePoints(List((5, 10), (6, 20), (0, 100)))

def isEven(num: Int): Boolean =
  num % 2 == 0

def isOdd(num: Int): Boolean =
  !isEven(num)

def sumOdds(list: List[Int]): Int =
  list match {
    case Nil => 0
    case head :: tail => (if(isOdd(head)) head else 0)+sumOdds(tail)
  }

sumOdds(List(1,2,3,4,5,6,7,8,9,10))

def sumOddPositions(list: List[Int]): Int =
  list match {
    case Nil => 0
    case head :: Nil => head
    case head :: tail => head + sumOddPositions(tail.tail)
  }

sumOddPositions(List(1,2,3,4,5,6,7,8,9,10))

def oddsPosition[A](list: List[A]): List[A] =
  list match {
    case Nil => Nil
    case head :: Nil => head :: Nil
    case head :: tail => head :: oddsPosition(tail.tail)
  }

oddsPosition(List(1,2,3,4,5,6,7,8,9,10))

def and(list: List[Boolean]): Boolean =
  list match {
    case head :: Nil => head
    case head :: tail => head && and(tail)
  }

and(List(true, true, true))
and(List(true, false))
and(List(true))

def concat[A](left: List[A], right: List[A]): List[A] =
  left match {
    case Nil => right
    case head :: tail => head :: concat(tail, right)
  }


concat(List(1,2,3), List(4,5,6))

