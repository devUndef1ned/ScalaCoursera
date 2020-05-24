package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceInner(res: Int, idx: Int): Boolean = {
      if (idx == chars.length) {
        res == 0
      } else {
        chars(idx) match {
          case '(' => balanceInner(res + 1, idx + 1)
          case ')' => if (res == 0) false else balanceInner(res - 1, idx + 1)
          case _ => balanceInner(res, idx + 1)
        }
      }
    }

    balanceInner(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) {
        (arg1, arg2)
      } else {
        chars(idx) match {
          case ')' => if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2) else traverse(idx + 1, until, arg1, arg2 + 1)
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val delta = (until - from) / 2
        val (l, r) = parallel(reduce(from, from + delta), reduce(from + delta, until))
        if (l._1 > r._2) {
          (l._1 - r._2 + r._1, l._2)
        } else {
          (r._1, r._2 - l._1 + l._2)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
