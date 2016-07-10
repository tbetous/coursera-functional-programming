object squareRootByNewton {
  def abs(x : Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrItr(guess : Double) : Double =
      if(isGoodEnough(guess)) guess
      else sqrItr(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrItr(1.0)
  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)
}
