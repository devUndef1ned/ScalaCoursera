package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(forall(x => x < 100, x => x < 100))
  println(exists(x => x < 100, x => x >= 101))
  println(exists(x => x < 100, x => x >= 99))
  println(map(_ < 100, 2*_)(50))
  println(map(_ < 100, 2*_)(49))
}
