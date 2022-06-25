package myTask

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import scala.math.pow

object myTask extends Properties("Solver") {
  def toList(range: Seq[Int], n: Int): List[Double] = range.map(x => (x, n)).collect(individual).toList

  def individual: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x, n)
    case (x, n) if x > n => n
  }
  def liftedIndividual: ((Int, Int)) => Option[Double] = individual.lift

  def toListLifted(range: Seq[Int], n: Int): List[Option[Double]] =
    range.foldLeft(List[Option[Double]]()) {
      (acc, x) => acc :+ liftedIndividual(x, n)
    }

  val list: List[Option[Double]] = toListLifted(-250 to 250, 4)

  property("correctIndividual") = forAll {
    (x: Int) =>
      val n = 4
      (x == n) ==> {
        liftedIndividual(x, n).isEmpty
      }
      (x < n) ==> {
        liftedIndividual(x, n).get == pow(x, n)
      }
      (x > n) ==> {
        liftedIndividual(x, n).get == n
      }
  }
}
