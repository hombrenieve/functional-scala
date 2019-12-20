def adjust(value: Int, interval: (Int, Int)): Int =
  value match {
    case p if (p < interval._1) => interval._1
    case p if (p > interval._2) => interval._2
    case p => p
  }

adjust(1, (-1, 2))
adjust(-10, (-1, 2))

def isLowerCaseLetter(c: Char): Boolean =
  c >= 'a' && c <= 'z'|| c == 'ñ'

isLowerCaseLetter('b')
isLowerCaseLetter('9')
isLowerCaseLetter('Ñ')

def toLowerCaseLetter(c: Char): Char =
  if(isLowerCaseLetter(c))
    c
  else
    (c-'A'+'a').toChar

toLowerCaseLetter('b')
toLowerCaseLetter('ñ')
toLowerCaseLetter('A')
toLowerCaseLetter('Z')
toLowerCaseLetter('Ñ')

def getMiddlePoint(interval:(Double, Double)) =
  (interval._2 + interval._1)/2

getMiddlePoint(10, 100)
getMiddlePoint(0,10)
getMiddlePoint(0,1)

def getLength(interval: (Double, Double)) =
  interval._2 - interval._1

def getHalfCenteredInterval(interval: (Double, Double)): (Double, Double) =
  (getMiddlePoint(interval)-getLength(interval)/4, getMiddlePoint(interval)+getLength(interval)/4)

getHalfCenteredInterval(0, 100)

def getFollowingDay(weekDay: Char): Char =
  weekDay match {
    case 's' => 'd'
    case 'd' => 'l'
    case 'l' => 'm'
    case 'm' => 'x'
    case 'x' => 'j'
    case 'j' => 'v'
    case 'v' => 's'
  }

getFollowingDay('m')
getFollowingDay('l')

def isChristmas(date: (Int, Int, Int)): Boolean =
  date._1 == 25 && date._2 == 12

isChristmas(20, 12, 2019)
isChristmas(25,12, 2020)

def cifras(num: Int): Int =
  num match {
    case _ if (num % 10 == num) => 1
    case _ => 1 + cifras (num / 10)
  }

cifras(99999)

def fibonacci(n: Int): Int =
  if (n < 2)
    1
  else
    fibonacci(n-1)+fibonacci(n-2)

fibonacci(1)
fibonacci(3)
fibonacci(5)

def mcd(pair: (Int, Int)): Int =
  pair match {
    case (a,b) if a == b => a
    case (a,b) if a > b => mcd(a-b, b)
    case (a,b) =>mcd(a, b-a)
  }

mcd(100, 50)
mcd(50, 25)
mcd(15, 10)

def mcm(pair: (Int, Int)): Int =
  pair match {
    case (a,b) => a*b/mcd(a,b)
  }

mcm(72, 50)

def isPrime(num: Int): Boolean = {
  def canBeDivided(num: Int, divisor: Int): Boolean =
    num % divisor == 0

  num match {
    case 1 => true

  }
}