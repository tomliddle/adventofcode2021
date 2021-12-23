import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day5TestSpec extends AnyFunSpec with Matchers {

  describe("Bonus") {

    it("should calculate updated value") {

      val matrix =
        Seq(Seq(1, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5))
      val matrix2 =
        Seq(
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 3, 4, 5)
        )

      val matrixResult =
        Seq(Seq(1, 2, 3, 4, 5), Seq(2, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5))

      Day5.updateValue(matrix, 1, 0).shouldEqual(matrixResult)

      Day5.getMaxGridSize(matrix).shouldEqual((4, 5))

      Day5.getMinMax(matrix.head.reverse).shouldEqual((3, 2, 5, 4))

      val matrixResult2 =
        Seq(Seq(2, 3, 3, 4, 5), Seq(1, 2, 3, 4, 5), Seq(1, 2, 3, 4, 5))
      Day5.doYUpdate(matrix, 0, 0, 1).shouldEqual(matrixResult2)

      val matrixResult3 =
        Seq(Seq(1, 2, 3, 5, 5), Seq(1, 2, 3, 5, 5), Seq(1, 2, 3, 4, 5))
      Day5.doXUpdate(matrix, 0, 3, 1).shouldEqual(matrixResult3)

      val matrixResult4 =
        Seq(
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 3, 4, 5),
          Seq(1, 2, 4, 4, 5),
          Seq(1, 2, 3, 5, 5),
          Seq(1, 2, 3, 4, 6)
        )
      Day5.doXYUpdate(matrix2, 2, 2, 4, 4).shouldEqual(matrixResult4)

      val str = Seq("0,0,5,5", "5,0,5,5")
      Day5.generateDay5b(str).shouldEqual(1)
    }

    it("should calculate updated value 2") {
      val str2 = Seq("0,0,5,5", "5,0,5,5", "0,5,5,5", "0,5,5,5")
      Day5.generateDay5b(str2).shouldEqual(6)
    }

    it("should calculate updated value 3") {
      val str2 = Seq("0,0,5,5", "5,0,5,5", "0,5,5,5", "0,5,5,5", "9,9,9,9")
      Day5.generateDay5b(str2).shouldEqual(6)
    }

  }
}
