import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

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

  describe("Bonus") {

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
  }
}
