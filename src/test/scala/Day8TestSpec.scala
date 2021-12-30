import Day4.{Board, Row}
import Day8.{calcLengthFives, calculateLengthSixs, getList, initialCalc}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

class Day8TestSpec extends AnyFunSpec with Matchers {

  val x: String =
    """|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce
      |""".stripMargin

  describe("Day 8") {

    it("should get long list") {
      Day8
        .getList(
          Seq(
            "eb cbgfae cabdf fedab efb adgcef cbgaefd egdb dbgefa eafgd | dfbae be gdafe gcefab"
          )
        )
        .map(_._2)
        .shouldEqual(Seq(Seq("dfbae", "be", "gdafe", "gcefab")))

    }

    it("calculate") {
      val list = x.split("\n")
      Day8
        .calculateDay8(list)
        .shouldEqual(26)

    }

    val str =
      "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

    it("get list") {
      val list = getList(Seq(str))
      list.shouldEqual(
        Seq(
          (
            ArraySeq(
              "acedgfb",
              "cdfbe",
              "gcdfa",
              "fbcad",
              "dab",
              "cefabd",
              "cdfgeb",
              "eafb",
              "cagedb",
              "ab"
            ),
            ArraySeq("cdfeb", "fcadb", "cdfeb", "cdbaf")
          )
        )
      )
    }

    it("initial calc") {
      val list = getList(Seq(str)).head._1
      val map = initialCalc(list)
      map.shouldEqual(
        Map(
          8 -> "acedgfb".sorted,
          7 -> "dab".sorted,
          4 -> "eafb".sorted,
          1 -> "ab"
        )
      )
    }

    it("calc length 5s") {
      val list = getList(Seq(str)).head._1
      val map = initialCalc(list)
      val map2 = calcLengthFives(map, list)
      map2.shouldEqual(
        Map(
          1 -> "ab",
          2 -> "gcdfa".sorted,
          3 -> "fbcad".sorted,
          4 -> "eafb".sorted,
          5 -> "cdfbe".sorted,
          7 -> "dab".sorted,
          8 -> "acedgfb".sorted
        )
      )
    }

    it("calc length 6s") {
      val list = getList(Seq(str)).head._1
      val map = initialCalc(list)
      val map2 = calcLengthFives(map, list)
      val map3 = calculateLengthSixs(map2, list)
      map3.shouldEqual(
        Map(
          1 -> "ab",
          2 -> "gcdfa".sorted,
          3 -> "fbcad".sorted,
          4 -> "eafb".sorted,
          5 -> "cdfbe".sorted,
          7 -> "dab".sorted,
          8 -> "acedgfb".sorted,
          6 -> "cdfgeb".sorted,
          0 -> "cagedb".sorted,
          9 -> "cefabd".sorted
        )
      )
    }
  }
}
