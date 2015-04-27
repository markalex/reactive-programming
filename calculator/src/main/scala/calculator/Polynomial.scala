package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      Math.pow(b.apply(), 2) - (4 * a.apply() * c.apply())
    }
  }
//(-b ± √Δ) / (2a)
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      delta.apply() match {
        case d if d < 0 => Set.empty
        case d if d == 0 => Set((-1 * b.apply()) / (2 * a.apply()))
        case d if d >= 0 => Set(
          ((-1 * b.apply()) + Math.sqrt(d)) / (2 * a.apply()),
          ((-1 * b.apply()) - Math.sqrt(d)) / (2 * a.apply()))
      }
    }
  }
}
