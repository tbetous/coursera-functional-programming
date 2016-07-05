package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s = singletonSet(1);
  val test = map(s, (x: Int) => x + 1)
  print(contains(test, 2))
}
