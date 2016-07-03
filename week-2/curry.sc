object curry {

  def mapReduce(f : Int => Int, combine : (Int, Int) => Int, id : Int)(a : Int, b : Int) = {
    def loop(a: Int, acc: Int) : Int = {
      if (a > b) acc
      else loop(a + 1, combine(f(a), acc))
    }

    loop(a, id)
  }

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  def sumWithMapReduce(f : Int => Int)(a : Int, b : Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  sum(x => x, 3, 5)
  sumWithMapReduce(x => x)(3, 5)

  def product(f : Int => Int)(a : Int, b : Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }
    loop(a, 1)
  }

  def productWithMapReduce(f : Int => Int)(a : Int, b : Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  product(x => x)(1,5)
  productWithMapReduce(x => x)(1,5)

  def cumulator(g : (Int, Int) => Int)(f : Int => Int)(a : Int, b : Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, g(acc,f(a)))
    }
    loop(a, 1)
  }

  def sumCumulator(f : Int => Int)(a : Int, b : Int) = cumulator((x, y) => x + y)(f : Int => Int)(a : Int, b : Int)

  def productCumulator(f : Int => Int)(a : Int, b : Int) = cumulator((x, y) => x * y)(f : Int => Int)(a : Int, b : Int)

  def fact(a : Int) = productCumulator(x => x)(1, a)

  fact(5)
}