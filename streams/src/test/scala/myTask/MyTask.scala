package myTask

class IndTest extends munit.FunSuite {
  var task = new PartialFunctionTask(3)

  test("x < n") {
    assert(task.lessN(2) == 8)
  }
  test("x > n") {
    assert(task.moreON(6) == 3)
  }
  test("x = n") {
    assert(task.equalN(3) == 0)
  }

}
