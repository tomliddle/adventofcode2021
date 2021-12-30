import Day4.{Board, Row}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day4TestSpec extends AnyFunSpec with Matchers {

  describe("Day4") {

    it("should calculate a 0 below 5") {

      val r1 = Row(Seq(11, 12, 13, 14, 15))
      val r2 = Row(Seq(21, 22, 23, 24, 25))
      val r3 = Row(Seq(31, 32, 33, 34, 35))
      val r4 = Row(Seq(41, 42, 43, 44, 45))
      val r5 = Row(Seq(51, 52, 53, 54, 55))
      val b = Board(Seq(r1, r2, r3, r4, r5))

      val r1a = Row(Seq(110, 120, 130, 140, 150))
      val r2a = Row(Seq(210, 220, 230, 240, 250))
      val r3a = Row(Seq(310, 320, 330, 340, 350))
      val r4a = Row(Seq(410, 420, 430, 440, 450))
      val r5a = Row(Seq(510, 520, 530, 540, 550))
      val ba = Board(Seq(r1a, r2a, r3a, r4a, r5a))

      val x = Day4.do4a(r5.numbers, Seq(ba, b))
      x shouldEqual (r1.numbers ++ r2.numbers ++ r3.numbers ++ r4.numbers).sum * 55

      val y = Day4.do4a(Seq(11, 21, 31, 41, 51), Seq(ba, b))
      y shouldEqual (r1.numbers.tail ++ r2.numbers.tail ++ r3.numbers.tail ++ r4.numbers.tail ++ r5.numbers.tail).sum * 51

      val z =
        Day4.do4a(Seq(11, 12, 31, 110, 210, 310, 410, 510), Seq(b, ba))
      z shouldEqual (r1a.numbers.tail ++ r2a.numbers.tail ++ r3a.numbers.tail ++ r4a.numbers.tail ++ r5a.numbers.tail).sum * 510
    }

    it("should calculate4b") {

      val r1 = Row(Seq(11, 12, 13, 14, 15))
      val r2 = Row(Seq(21, 22, 23, 24, 25))
      val r3 = Row(Seq(31, 32, 33, 34, 35))
      val r4 = Row(Seq(41, 42, 43, 44, 45))
      val r5 = Row(Seq(51, 52, 53, 54, 55))
      val b = Board(Seq(r1, r2, r3, r4, r5))

      val r1a = Row(Seq(110, 120, 130, 140, 150))
      val r2a = Row(Seq(210, 220, 230, 240, 250))
      val r3a = Row(Seq(310, 320, 330, 340, 350))
      val r4a = Row(Seq(410, 420, 430, 440, 450))
      val r5a = Row(Seq(510, 520, 530, 540, 550))
      val ba = Board(Seq(r1a, r2a, r3a, r4a, r5a))

      val xa = Day4.do4b(r5.numbers, Seq(ba, b))
      xa shouldEqual (r1.numbers ++ r2.numbers ++ r3.numbers ++ r4.numbers).sum * 55

      val x = Day4.do4b(r1a.numbers ++ r5.numbers, Seq(ba, b))
      x shouldEqual (r1.numbers ++ r2.numbers ++ r3.numbers ++ r4.numbers).sum * 55

      val y =
        Day4.do4b(r1a.numbers ++ Seq(11, 21, 31, 41, 51), Seq(ba, b))
      y shouldEqual (r1.numbers.tail ++ r2.numbers.tail ++ r3.numbers.tail ++ r4.numbers.tail ++ r5.numbers.tail).sum * 51

      val z =
        Day4.do4b(
          r5.numbers ++ Seq(11, 12, 31, 110, 210, 310, 410, 510),
          Seq(b, ba)
        )
      z shouldEqual (r1a.numbers.tail ++ r2a.numbers.tail ++ r3a.numbers.tail ++ r4a.numbers.tail ++ r5a.numbers.tail).sum * 510
    }

  }
}
