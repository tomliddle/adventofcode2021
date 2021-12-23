import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day7TestSpec extends AnyFunSpec with Matchers {

  describe("Bonus") {

    it("should get long list") {
      Day7
        .toLongList(List("1,2,3,4,5,6"))
        .shouldEqual(List(1L, 2L, 3L, 4L, 5L, 6L))
    }

    it("should add crab costs") {
      Day7
        .toLongList(List("1,2,3,4,5,6"))
        .shouldEqual(List(1L, 2L, 3L, 4L, 5L, 6L))
    }

    it("should add crab costs") {
      Day7
        .toLongList(List("1,2,3,4,5,6"))
        .shouldEqual(List(1L, 2L, 3L, 4L, 5L, 6L))
    }

    it("should get calculate") {
      Day7
        .calculate(List(1L, 2L, 3L))
        .shouldEqual(3)
    }
  }
}
