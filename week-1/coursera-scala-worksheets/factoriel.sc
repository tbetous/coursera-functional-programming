object factoriel {
  def factoriel(x : Int) : Int = {
    def factorielItr(x : Int, acc : Int) : Int =
      if(x == 0) acc else factorielItr(x - 1, acc * x)

    factorielItr(x, 1)
  }                                         //> factoriel: (x: Int)Int

  factoriel(3)                              //> res0: Int = 6
}