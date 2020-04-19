package quickcheck

object quickcheck {

  def main(args: Array[String]): Unit = {
    val list1 = List(1, 2, 3)
    val list2 = List(3, 2, 1)
    println(list1 == list2)
  }
}
