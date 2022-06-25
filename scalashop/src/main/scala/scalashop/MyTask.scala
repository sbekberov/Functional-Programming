package scalashop

import scalashop.HorizontalBoxBlurRunner.standardConfig

import scala.math.pow

object MyTask extends App{

  def toList(range: Seq[Int], n: Int): List[Double] =
    range.map(x => (x, n)).collect(individual).toList

  def individual: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x, n)
    case (x, n) if x > n => n
  }

  def getResult(range: Seq[Int], n: Int, numTasks: Int): List[Double] = {
    val elemsPerTask: Int = range.length / numTasks max 1
    val startPoints = range.indices by elemsPerTask


    val tasks = startPoints.map(t => {
      task {
        toList(range.slice(t, t + elemsPerTask), n)
      }
    })

    var result = List[Double]()
    tasks.map(t => t.join()).foreach(v => {
      result = result ::: v
    })
    result
  }

  val n = 1
  val result = toList(-250 until 250, n)
  //  println(result + "\n")

  val parResult = getResult(-250 until 250, n, 3)
  //  println(parResult)

  val seqtime = standardConfig measure {
    MyTask.result
  }
  val partime = standardConfig measure {
    MyTask.parResult
  }
  println(s"speedup: ${seqtime.value / partime.value}")
}