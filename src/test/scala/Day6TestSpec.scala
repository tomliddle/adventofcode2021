import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day6TestSpec extends AnyFunSpec with Matchers {

  describe("Day 6") {

    it("should get list") {
      Day6
        .getList(List("1,2,3,4,5,6,7,9999"))
        .shouldEqual(List(1, 2, 3, 4, 5, 6, 7, 9999))

    }

    it("should  updatefish value 2") {
      Day6
        .updateFish(Map(1 -> 2, 2 -> 2, 3 -> 2, 4 -> 2))
        .shouldEqual(Map(0 -> 2, 1 -> 2, 2 -> 2, 3 -> 2))
    }

    it("should  updatefish value 3") {
      Day6
        .updateFish(Map(0 -> 2, 1 -> 2, 2 -> 2, 3 -> 2))
        .shouldEqual(Map(6 -> 2, 0 -> 2, 1 -> 2, 2 -> 2, 8 -> 2))
    }

    it("should  updatefish value 4") {
      Day6
        .updateFish(Map(3 -> 3))
        .shouldEqual(Map(2 -> 3))
    }

    it("should  updatefish value 5") {
      Day6
        .updateFish(
          Day6.updateFish(Day6.updateFish(Day6.updateFish(Map(3 -> 3))))
        )
        .shouldEqual(Map(6 -> 3, 8 -> 3))
    }

    it("should calculate 18updated value 1") {
      val map = Map(3 -> 2L, 4 -> 1L, 1 -> 1L, 2 -> 1L)
      Day6.calculate(18, map).shouldEqual(26)
    }

    it("should calculate updated value 1") {
      //3,4,3,1,2
      val map = Map(3 -> 2L, 4 -> 1L, 1 -> 1L, 2 -> 1L)
      Day6.calculate(80, map).shouldEqual(5934)
    }

    it("should calculate updated value 2") {
      //3,4,3,1,2
      val map = Map(3 -> 2L, 4 -> 1L, 1 -> 1L, 2 -> 1L)
      Day6.calculate(1, map).shouldEqual(5)
    }

    it("should calculate updated value 3") {
      //3,4,3,1,2
      val map = Map(0 -> 2L, 1 -> 1L, 2 -> 1L, 3 -> 1L)
      Day6.calculate(1, map).shouldEqual(7)
    }

    it("should calculate updated value 4") {
      val map = Map(0 -> 2L, 1 -> 1L, 2 -> 1L, 3 -> 1L)
      Day6.calculate(2, map).shouldEqual(8)
    }

    it("should listToMap") {
      val map = Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1)
      Day6.listToMap(List(1, 2, 3, 4, 5)).shouldEqual(map)
    }

    it("should listToMap 2") {
      val map = Map(1 -> 2, 2 -> 2, 3 -> 2, 4 -> 2, 5 -> 2)
      Day6.listToMap(List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)).shouldEqual(map)
    }

    it("count fish") {
      val map = Map(1 -> 2L, 2 -> 2L, 3 -> 2L, 4 -> 2L, 5 -> 2L)
      Day6.countFish(map).shouldEqual(10)
    }
  }
}
