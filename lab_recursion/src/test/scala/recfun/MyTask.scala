package recfun

import org.scalatest.funsuite.AnyFunSuite

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MyTask extends AnyFunSuite {

  import Main.funcSolution

  test("funcSolution: x < n") {
    assert(funcSolution(2, 3) === 8)
    assert(funcSolution(-2, 3) === -8)
    assert(funcSolution(-4, -2) === 0.0625)
    assert(funcSolution(-2, 0) === 1)
  }

  test("funcSolution: x > n") {
    assert(funcSolution(3, 2) === 2)
    assert(funcSolution(3, -2) === -2)
    assert(funcSolution(-2, -3) === -3)
    assert(funcSolution(2, 0) === 0)
  }

  test("funcSolution: x == n") {
    assertThrows[IllegalArgumentException](funcSolution(2, 2))
    assertThrows[IllegalArgumentException](funcSolution(-2, -2))
    assertThrows[IllegalArgumentException](funcSolution(0, 0))
  }
}