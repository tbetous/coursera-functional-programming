object tree {
  abstract class IntSet{
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other : IntSet) : IntSet
  }

  object Empty extends IntSet {
    def contains(x : Int) = false
    def incl(x: Int) = new NonEmpty(x, Empty, Empty)
    def union(other : IntSet) = other
    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x : Int) =
      if(x > elem) right.contains(x)
      else if (x < elem) left.contains(x)
      else true

    def incl(x: Int) =
      if(x > elem) new NonEmpty(elem, left, right.incl(x))
      else if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else this

    def union(other : IntSet) =
      ((left union right) union other) incl elem

    override def toString =
      "{" +  left + elem + right + "}"
  }

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = new NonEmpty(6, t1, Empty)
  t2.incl(3)
    .incl(8)
    .incl(12)
  val t3 = new NonEmpty(4, Empty, Empty)
  t2.union(t3)
}

