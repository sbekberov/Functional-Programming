package myTask

class PartialFunctionTask (n: Int) {
  def ToList(range: Seq[Int]): List[Double] = range.collect(moreON orElse lessN orElse equalN).toList

  def pow(base: Double, exp: Int, acc: Double = 1): Double = {
    if (exp == 0 || base == 1) {
      acc
    } else if (exp < 0) {
      pow(1 / base, -exp - 1, acc / base)
    } else {
      pow(base, exp - 1, acc * base)
    }
  }

  val moreON: PartialFunction[Int,Double] = {
    case x if x > n => n
  }
  val lessN: PartialFunction[Int, Double] = {
    case x if x < n => pow(x, n)
  }
  val equalN: PartialFunction[Int, Double] = {
    case x if x == n => 0
  }
}