import Day4.{Board, Row}
import Day8.{calcLengthFives, calculateLengthSixs, getList, initialCalc}
import Day9.Matrix
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

class Day9TestSpec extends AnyFunSpec with Matchers {

  val x: String =
    """|2199943210
       |3987894921
       |9856789892
       |8767896789
       |9899965678
      |""".stripMargin

  val y: String =
    """|9999999990
         |9999999990
         |9999999990
         |9999999990
         |9999999990
         |""".stripMargin

  val z: String =
    """|9999999990
       |9999999991
       |9999999990
       |9999999991
       |9999999990
       |""".stripMargin

  val zz: String =
    """|9999999993
       |1999999991
       |9999499990
       |7999999991
       |9999999990
       |""".stripMargin

  val l: Array[String] = x.split("\n")
  val matrix = Day9
    .getMatrix(l)
  given Matrix = matrix

  describe("Day 9") {

    it("should get matrix") {
      matrix.size.shouldEqual(5)
      matrix.head.shouldEqual(ArraySeq(2, 1, 9, 9, 9, 4, 3, 2, 1, 0))
    }

    it("is min") {
      Day9.isMin((1, 0)).shouldEqual(Some((1, 0)))
      Day9.isMin((1, 1)).shouldEqual(None)
    }

    it("calc mins") {
      Day9.calcMins(Seq((1, 0))).shouldEqual(Seq((1, 0)))
      Day9.calcMins(Seq((0, 0))).shouldEqual(Seq.empty)
    }

    it("calc low points ") {
      Day9.lowPoints.shouldEqual(Seq((1, 0), (9, 0), (2, 2), (6, 4)))
    }

    it("calc scores") {
      Day9.scores(Day9.lowPoints.toSeq).shouldEqual(Seq(2, 1, 6, 6))
    }

    it("calc day 9 ") {
      Day9.calculateDay9(l).shouldEqual(15)
    }

    it("calc day 9 2") {
      Day9.calculateDay9(zz.split("\n")).shouldEqual(17)
    }

    it("calc day 9 3") {
      val seq = zz.split("\n")
      val matrix = Day9
        .getMatrix(seq)

      given Matrix = matrix

      Day9.lowPoints.shouldEqual(
        Seq((0, 1), (4, 2), (9, 2), (0, 3), (9, 4))
      )
    }

    // 9, 9
    // 0, 9
    // 9, 9
    it("calc mins for small matrix") {
      val matrix = Seq(Seq(9, 9), Seq(0, 9), Seq(9, 9))
      given Matrix = matrix
      Day9.calcMins(Seq((0, 1), (1, 1))).shouldEqual(Seq((0, 1)))
    }
  }
}
