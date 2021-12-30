import scala.math.abs
import cats.implicits._

object Day9 {

  type Matrix = Seq[Seq[Int]]
  type Coord = (Int, Int)

  extension (c: Char)
    def parseInt: Int = Integer.parseInt(c.toString)

  extension (i: Int)
    def maxVal(max: Int) = if (i == max) max else i + 1

  def getMatrix(list: Seq[String]): Matrix =
    list.map { _.map(_.parseInt) }

  def isMin(coord: Coord)(using matrix: Matrix): Option[Coord] =
    val xMax = coord._1.maxVal(matrix.head.size -1)
    val yMax = coord._2.maxVal(matrix.size - 1)
    val value = matrix(coord._2)(coord._1)
    val x = coord._1
    val y = coord._2

    val left =  x == 0 || value < matrix(y)(x - 1)
    val right = x == xMax || value < matrix(y)(x + 1)
    val top = y == yMax || value < matrix(y + 1)(x)
    val bottom = y == 0 || value < matrix(y - 1)(x)
    if (left && right && top && bottom) coord.some
    else None

  def calcMins(row: Seq[Coord])(using matrix: Matrix): Seq[Coord] =
    row.flatMap(isMin)

  def riskLevel(coord: Coord)(using matrix: Matrix): Int =
    matrix(coord._2)(coord._1) + 1

  def lowPoints(using matrix: Matrix): Seq[Coord] =
    matrix.zipWithIndex.flatMap { case (row, yVal) =>
      row.zipWithIndex.flatMap { case (value, xVal) =>
        isMin((xVal, yVal))
      }
    }

  def scores(list: Seq[Coord])(using matrix: Matrix): Seq[Int] =
    list.map(riskLevel)

  def calculateDay9(list: Seq[String]): Int = {
    val matrix = getMatrix(list)
    given Matrix = matrix

    val lowPointSeq = lowPoints
    scores(lowPointSeq).sum
  }

  def calculateDay9b(list: Seq[String]): Int = {
    1 // TODO...
  }
}
