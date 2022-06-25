import myTask.myTask
import org.junit.{Assert, Rule, Test}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Test.{Failed, PropException, Result, check}

class myTask {

  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq: _*)

  @Test def `individual task works correctly`(): Unit =
    Assert.assertTrue(
      check(asProp(myTask))(identity).passed
    )

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}