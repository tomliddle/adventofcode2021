object Day2 {

  def generateDay2(list: Seq[String]): Int = {
    val (horizontal, depty) = list.foldLeft(0, 0) { (acc, s) =>
      val line = s.split(" ")
      val cmd = line.head.trim
      val value = line(1).toInt
      cmd match {
        case "up"      => (acc._1, acc._2 - value)
        case "forward" => (acc._1 + value, acc._2)
        case "down"    => (acc._1, acc._2 + value)
        case _         => throw new RuntimeException("wrong input")
      }
    }
    horizontal * depty
  }

  def generateDay2b(list: Seq[String]): Int = {
    val (horizontal, depth, aim) = list.foldLeft(0, 0, 0) { (acc, s) =>
      val line = s.split(" ")
      val cmd = line.head.trim
      val value = line(1).toInt
      cmd match {
        case "up"      => (acc._1, acc._2, acc._3 - value)
        case "forward" => (acc._1 + value, acc._2 + (acc._3 * value), acc._3)
        case "down"    => (acc._1, acc._2, acc._3 + value)
        case _         => throw new RuntimeException("wrong input")
      }
    }
    horizontal * depth
  }
}
