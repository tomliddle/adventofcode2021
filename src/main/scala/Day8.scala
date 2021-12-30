import scala.math.abs

object Day8 {

  def getList(list: Seq[String]): Seq[(Seq[String], Seq[String])] =
    list.map { line =>
      val arr = line.split("\\|")
      (arr.head.trimStr, arr.drop(1).head.trimStr)
    }

  def calculateDay8(list: Seq[String]): Long = {
    val lengths = List(2, 3, 4, 7)
    val l = getList(list).flatMap(_._2)
    l.count { str => lengths.contains(str.length) }
  }

  extension (s: String)
    def isOne: Boolean = s.length == 2
    def isFour: Boolean = s.length == 4
    def isSeven: Boolean = s.length == 3
    def isEight: Boolean = s.length == 7
    def parseInt: Int = Integer.parseInt(s)
    def trimStr: Seq[String] = s.trim.split(" ").toSeq
    def containsChars(t: String): Boolean =
      t.forall(chr => s.contains(chr))


  def initialCalc(ss: Seq[String]): Map[Int, String] =
    ss.foldLeft(Map.empty[Int, String]) { (result, current) =>
      current.length match {
        case 2 => result + (1 -> current.sorted)
        case 4 => result + (4 -> current.sorted)
        case 3 => result + (7 -> current.sorted)
        case 7 => result + (8 -> current.sorted)
        case _ => result // unknown at the moment
      }
    }

  def calcLengthFives(mapData: Map[Int, String], seqData: Seq[String]): Map[Int, String] =
    val topLeftPlusMiddle = mapData(4).diff(mapData(1))
    seqData.filter(_.length == 5).toSet.foldLeft(mapData) { (result, current) =>
      current match {
        case x if x.containsChars(mapData(7)) => result + (3 -> current.sorted) // 3 doesn't contain all of 7's segments
        case x if x.containsChars(topLeftPlusMiddle) => result + (5 -> current.sorted) // 5 contains top left and middle segments
        case _ => result + (2 -> current.sorted) // default size five
      }
    }

  def calculateLengthSixs(mapData: Map[Int, String], seqData: Seq[String]): Map[Int, String] =
    seqData.filter(_.length == 6).toSet.foldLeft(mapData) { (result, current) =>
      current match {
        case x if !x.containsChars(mapData(7)) => result + (6 -> current.sorted) // doesn't contain all of 7's segments
        case x if !x.containsChars(mapData(4)) => result + (0 -> current.sorted) // contains all of 4's chars
        case _ => result + (9 -> current.sorted)
      }
    }

  def calculateDay8b(list: Seq[String]): Int = {
    getList(list).map { row =>
      val seqData: Seq[String] = row._1
      val mapData = initialCalc(seqData)
      val mapData2 = calcLengthFives(mapData, seqData)
      val mapData3 = calculateLengthSixs(mapData2, seqData)

      val code = row._2
      code.flatMap { token =>
        mapData3.find(_._2 == token.sorted).map(_._1.toString)
      }.mkString("").parseInt
    }.sum
  }
}
