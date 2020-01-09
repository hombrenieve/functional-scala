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
  n match {
    case 1 => 1
    case 2 => 1
    case _ => fibonacci(n-1)+fibonacci(n-2)
  }

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

def pow(base: Double, exp: Int): Double =
  exp match {
    case 1 => base
    case _ => base*pow(base, exp-1)
  }

pow(2,3)

def addition(a:Int, b:Int):Int =
  b match {
    case 0 => a
    case _ => 1+addition(a, b-1)
  }

addition(2,5)


def resto(dividendo: Int, divisor: Int): Int =
  dividendo match {
    case d if(d < divisor) => d
    case _ => resto(dividendo-divisor, divisor)
  }

resto(3, 8)
resto(10, 2)
resto(5, 2)


def isPrime(num: Int): Boolean = {
  def cantBeDivided(num: Int, divisor: Int): Boolean =
    divisor == 1 || ((num % divisor) match {
      case 0 => false
      case r if (r != 0) => cantBeDivided(num, divisor-1)
    })
  num match {
    case 1 => true
    case _ => cantBeDivided(num, num-1)
  }

}

isPrime(1)
isPrime(3)
isPrime(10)
isPrime(15)
isPrime(23)

def pot(base: Int, exp: Int): Int = {
  def pot2(base: Int, exp: Int, acc: Int): Int =
    exp match {
      case 0 => acc
      case _ => pot2(base, exp-1, base*acc)
    }
  pot2(base, exp, 1)
}

pot(2, 5)
pot(5, 2)