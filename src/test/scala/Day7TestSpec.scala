import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day7TestSpec extends AnyFunSpec with Matchers {

  describe("Day 7") {

    it("should get long list") {
      Day7
        .toLongList(List("1,2,3,4,5,6"))
        .shouldEqual(List(1L, 2L, 3L, 4L, 5L, 6L))
    }

    it("should add new crab costs") {
      val fn = (position: Long) => Day7.sevenACrabCost(4L, position)
      Day7
        .addCrabCosts(List(1L, 2L, 3L, 4L, 5L, 6L), fn)
        .shouldEqual(List(5L, 5L, 5L, 5L, 5L, 7L))
    }

    it("should calculate 2") {
      val fn = (position: Long) => Day7.sevenACrabCost(5L, position)
      Day7
        .addCrabCosts(List.fill(10)(0L), fn)
        .shouldEqual(List(5, 4, 3, 2, 1, 0, 1, 2, 3, 4))
    }

    it("should calculate") {
      val fn =
        (crabPos: Long) =>
          (position: Long) => Day7.sevenACrabCost(crabPos, position)
      Day7
        .calculate(List(1L, 2L, 3L), fn)
        .shouldEqual(2)
    }

    it("should calculate part a") {
      val fn =
        (crabPos: Long) =>
          (position: Long) => Day7.sevenACrabCost(crabPos, position)
      Day7
        .calculate(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14).map(_.toLong), fn)
        .shouldEqual(37)
    }

    it("should calculate part b") {
      val fn =
        (crabPos: Long) =>
          (position: Long) => Day7.sevenBCrabCost(crabPos, position)
      Day7
        .calculate(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14).map(_.toLong), fn)
        .shouldEqual(168)
    }
  }
}
