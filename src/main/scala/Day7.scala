import scala.math.abs

object Day7 {

  def toLongList(list: List[String]): List[Long] =
    list.head.split(",").toList.map(s => Integer.parseInt(s).toLong)

  def sevenACrabCost(crabPos: Long, position: Long): Long = abs(
    crabPos - position
  )

  def sevenBCrabCost(crabPos: Long, position: Long): Long = {
    def calc(counter: Long, result: Long = 0): Long =
      if (counter == 0) result
      else calc(counter - 1, result + counter)

    calc(abs(crabPos - position))
  }

  def addCrabCosts(
      list: List[Long],
      crabCostFn: (Long) => Long
  ): List[Long] =
    list.zipWithIndex.map { case (currVal, currPos) =>
      currVal + crabCostFn(currPos)
    }

  def calculate(
      list: List[Long],
      fn: Long => Long => Long
  ): Long = {
    val max = list.max.toInt

    list
      .foldLeft(List.fill(max)(0L)) { (result, current) =>
        addCrabCosts(result, fn(current))
      }
      .min
  }

  def calculateDay7(list: Seq[String]): Long = {
    val l = toLongList(list.toList)
    calculate(l, crabPos => position => sevenACrabCost(crabPos, position))
  }

  def calculateDay7b(list: Seq[String]): Long = {
    val l = toLongList(list.toList)
    calculate(l, crabPos => position => sevenBCrabCost(crabPos, position))
  }

}
