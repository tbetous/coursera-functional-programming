object list {
  trait List[T] {
    def isEmpty() : Boolean
    def head() : T
    def tail() : List[T]
  }

  class Cons[T](val head : T, val tail : List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T] extends List[T] {
    def isEmpty = true
    def head() = throw new NoSuchElementException("Nil.head")
    def tail() = throw new NoSuchElementException("Nil.tail")
  }

  def singleton[T](elmt : T) : List[T] = new Cons[T](elmt, new Nil[T])

  def nth[T](n : Int, list : List[T]) : T =
    if (list.isEmpty()) throw new IndexOutOfBoundsException
    else if (n == 0) return list.head()
    else return nth(n - 1, list.tail())

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  nth(-1, list)
}
