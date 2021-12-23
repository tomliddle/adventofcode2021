import scala.annotation.tailrec
import scala.math.abs

object Day6 {

  def getList(numberList: List[String]): List[Long] =
    numberList.head.split(",").toList.map(no => Integer.parseInt(no))

  def updateFish(numbers: Map[Int, Long]): Map[Int, Long] = {
    numbers.foldLeft(Map.empty[Int, Long]) { case (result, (k, v)) =>
      if (k == 0) {
        val existing6 = result.getOrElse(6, 0L)
        val plus8 = result + (8 -> v)
        plus8 + (6 -> (existing6 + v))
      } else {
        val newKey: Int = k - 1
        val existingVal: Long = result.getOrElse(newKey, 0L)
        result + (newKey -> (v + existingVal))
      }
    }
  }

  def countFish(numberList: Map[Int, Long]): Long =
    numberList.values.sum

  @tailrec
  def calculate(daysLeft: Long, map: Map[Int, Long]): Long = {
    if (daysLeft == 0) countFish(map)
    else {
      val updatedMap = updateFish(map)
      calculate(daysLeft - 1, updatedMap)
    }
  }

  def listToMap(value: List[Long]): Map[Int, Long] = {
    value.groupMapReduce(x => x.toInt)(_ => 1L)((a, b) => a + b)
  }

  def generateDay6(numberList: Seq[String]): Long = {
    val list = getList(numberList.toList)
    val map = listToMap(list)

    calculate(80, map)
  }

  def generateDay6b(numberList: List[String]): Long = {
    val list = getList(numberList)
    val map = listToMap(list)

    calculate(256, map)
  }
}
