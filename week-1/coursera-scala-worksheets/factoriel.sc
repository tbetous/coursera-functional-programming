object factoriel {
  def factoriel(x : Int) : Int = {
    def factorielItr(x : Int, acc : Int) : Int =
      if(x == 0) acc else factorielItr(x - 1, acc * x)

    factorielItr(x, 1)
  }

  factoriel(3)
}