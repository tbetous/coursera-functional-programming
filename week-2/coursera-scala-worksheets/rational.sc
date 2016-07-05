object rational {
  class Rational(_numer : Int, _denom : Int) {
    private def gcd(a : Int, b : Int): Int = if(b == 0) a else gcd(b, a%b)
    private val g = gcd(_numer, _denom)
    def numer = _numer / g
    def denom = _denom / g

    def this(_numer : Int) = this(_numer, 1)

    def < (that : Rational) : Boolean = this.numer * that.denom < that.numer * this.denom

    def max(that : Rational) : Rational = if (this < that) that else this

    def unary_- = new Rational(-numer, denom)

    def + (that : Rational) : Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    def sub(that: Rational) = this + -that

    override def toString = numer + "/" + denom;
  }

  val r1 = new Rational(4, 8)
  val r2 = new Rational(2, 3)
  val r3 = r1 + r2
  val r4 = r1.sub(r2)
  val r5 = new Rational(4)
  r2 < r1
  r2 max r1
}