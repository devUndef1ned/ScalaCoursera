package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def solution(n: Double, withAdding: Boolean): Double = {
      (-1 * b() + ((if (withAdding) 1 else -1) * Math.sqrt(n))) / (2 * a())
    }

    Signal(
      delta() match {
        case n if n < 0 => Set()
        case n if n == 0 => Set(-1 * b() / (2 * a()))
        case n => Set(solution(n, true), solution(n, false))
      }
    )
  }
}
