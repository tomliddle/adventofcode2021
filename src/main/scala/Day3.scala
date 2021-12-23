object Day3 {
  def generateDay3(list: Seq[String]): Int = {
    val onesCount: Seq[Int] = list.foldLeft(Seq.fill(12)(0)) { (curr, row) =>
      row.zipWithIndex.map { case (chr, idx) =>
        curr(idx) + Integer.parseInt(chr.toString)
      }
    }
    val binaryGammaStr =
      onesCount.map(x => if (x > list.size / 2) "1" else "0").mkString
    val binaryEpsilonStr =
      onesCount.map(x => if (x < list.size / 2) "1" else "0").mkString
    val gamma = Integer.parseInt(binaryGammaStr, 2)
    val epsilon = Integer.parseInt(binaryEpsilonStr, 2)

    gamma * epsilon
  }

  def generateDay3b(list: Seq[String]): Int = {

    def filter(
        idx: Int,
        filterOn: Char,
        list: Seq[String],
        fn: (Int, Int) => Boolean
    ): String = {
      if (list.size == 1 || idx == 12) list.head
      else {
        val (wantedList, discardList) = list.partition { str =>
          str(idx) == filterOn
        }
        val biggestList =
          if (fn(wantedList.size, discardList.size)) wantedList else discardList
        filter(idx + 1, filterOn, biggestList, fn)
      }
    }

    val binaryO2Rating = filter(0, '1', list, (x, y) => x >= y)
    val o2Rating = Integer.parseInt(binaryO2Rating, 2)

    val binaryScrubberRating = filter(0, '0', list, (x, y) => x <= y)
    val scrubberRating = Integer.parseInt(binaryScrubberRating, 2)

    o2Rating * scrubberRating
  }
}
