object Day1 {
  def generateDay1(list: Seq[Int]): Int =
    list.sliding(2).foldLeft(0) { (acc, curr) =>
      if (curr(1) > curr.head) acc + 1
      else acc
    }

  def generateDay1b(list: Seq[Int]): Int =
    list.sliding(4).foldLeft(0) { (acc, curr) =>
      val prev = curr.take(3).sum
      val next = curr.drop(1).sum
      if (next > prev) acc + 1
      else acc
    }
}
