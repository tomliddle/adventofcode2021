import scala.math.abs

object Day8 {

  val lengths = List(2, 3, 4, 7)

  def trimStr(str: String): Seq[String] = str.trim.split(" ").toSeq

  def getList(list: Seq[String]): Seq[(Seq[String], Seq[String])] =
    list.map { line =>
      val arr = line.split("\\|")
      (trimStr(arr.head), trimStr(arr.drop(1).head))
    }

  def calculateDay8(list: Seq[String]): Long = {
    val l = getList(list).map(_._2)
    l.count { str => lengths.contains(str.length) }
  }

  def calculateDay8b(list: Seq[String]): Long = {
    1L
  }

}
