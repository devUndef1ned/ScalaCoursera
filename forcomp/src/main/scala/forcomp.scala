import java.util

import forcomp.Anagrams

object test {

  def main(args: Array[String]): Unit = {
    val w = "abcd"
    val result = combine(w)
    println(result)
  }

  def combine(input: String): List[String] = {

    input.toList.foldLeft(List[String]())((list, c) => String.valueOf(c) :: list ::: list.map(_ + c) )
  }
}
