def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a>b) 0
  else f(a) + sum(f)(a+1, b)
}

sum((x: Int) => x)(1,3)

def product(f: Int => Int)(a: Int, b:Int) : Int = {
  if(a>b) 1
  else f(a) * product(f)(a+1, b)
}

product(x=>x)(1,2)

def factorial(n: Int) : Int = product(x=>x)(1, n)

def series(u: Int, g: (Int, Int) => Int, f: Int => Int)(a: Int, b:Int) : Int = {
  if(a>b) u
  else g(f(a), series(u, g, f)(a+1, b))
}

series(0, (x:Int, y:Int) => x+y, x=>x)(1,3)

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  val g = gcd(x, y)
  val numer = x
  val denom = y

  private def gcd(a: Int, b: Int) : Int = if(b == 0) a else gcd(b, a % b)
  override def toString = this.numer / g + "/" + this.denom / g
}

val x = new Rational(4,2)