import scala.math.abs

object Day7 {

  def toLongList(list: List[String]): List[Long] =
    list.head.split(",").toList.map(s => Integer.parseInt(s).toLong)

  def crabCost(crabPos: Long, position: Int): Long = abs(crabPos - position)

  def addCrabCosts(list: List[Long], crabPos: Long): List[Long] =
    list.zipWithIndex.map { case (crabPos, position) =>
      crabCost(crabPos, position)
    }

  def calculate(list: List[Long]): Long = {
    val max = list.max.toInt

    list
      .foldLeft(List.fill(max)(0L)) { (result, current) =>
        addCrabCosts(result, current)
      }
      .min
  }

  def calculateDay7(list: List[String]): Long = {
    val l = toLongList(list)
    calculate(l)
  }

}
