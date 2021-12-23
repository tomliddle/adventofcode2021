import scala.math.abs

object Day5 {

  def getLines(numberList: Seq[String]): Seq[Seq[Int]] = {
    numberList.map { line =>
      line.split("->|,").map(token => Integer.parseInt(token.trim)).toSeq
    }
  }

  def getMaxGridSize(lines: Seq[Seq[Int]]) = {
    val x: Int = (lines.map(_.head) ++ lines.map(row => row(2))).max + 1
    val y: Int = (lines.map(row => row(1)) ++ lines.map(row => row(3))).max + 1
    (x, y)
  }

  def updateValue(matrix: Seq[Seq[Int]], x: Int, y: Int): Seq[Seq[Int]] = {
    val value = matrix(x)(y)
    val updatedRow: Seq[Int] = matrix(x).updated(y, value + 1)
    matrix.updated(x, updatedRow)
  }

  def getMinMax(curr: Seq[Int]) = {
    val xMin = scala.math.min(curr.head, curr(2))
    val xMax = scala.math.max(curr.head, curr(2))
    val yMin = scala.math.min(curr(1), curr(3))
    val yMax = scala.math.max(curr(1), curr(3))
    (xMin, yMin, xMax, yMax)
  }

  def doYUpdate(
      current: Seq[Seq[Int]],
      x: Int,
      yMin: Int,
      yMax: Int
  ) = {
    (yMin to yMax).foldLeft(current) { (newMatrix, currentY) =>
      updateValue(newMatrix, x, currentY)
    }
  }

  def doXUpdate(
      current: Seq[Seq[Int]],
      xMin: Int,
      y: Int,
      xMax: Int
  ) = {
    (xMin to xMax).foldLeft(current) { (newMatrix, currentX) =>
      updateValue(newMatrix, currentX, y)
    }
  }

  def doXYUpdate(
      current: Seq[Seq[Int]],
      x1: Int,
      y1: Int,
      x2: Int,
      y2: Int
  ) = {

    val xRange = x1 to x2
    val yRange = y1 to y2
    xRange.zip(yRange).foldLeft(current) { (newMatrix, curr) =>
      updateValue(newMatrix, curr._1, curr._2)
    }
  }

  def generateDay5(numberList: Seq[String]): Int = {
    val lines = getLines(numberList)
    val (xmax, ymax) = getMaxGridSize(lines)

    val matrix = lines.foldLeft(Seq.fill(xmax, ymax)(0)) { (current, curr) =>
      val (xMin, yMin, xMax, yMax) = getMinMax(curr)

      if (xMin == xMax) doYUpdate(current, xMin, yMin, yMax)
      else if (yMin == yMax) doXUpdate(current, xMin, yMin, xMax)
      else current
    }

    matrix.map(row => row.count(_ >= 2)).sum
  }

  def generateDay5b(numberList: Seq[String]): Int = {
    val lines = getLines(numberList)
    val (xmax, ymax) = getMaxGridSize(lines)

    val matrix = lines.foldLeft(Seq.fill(xmax, ymax)(0)) { (current, curr) =>
      val (xMin, yMin, xMax, yMax) = getMinMax(curr)

      if (xMin == xMax) doYUpdate(current, xMin, yMin, yMax)
      else if (yMin == yMax) doXUpdate(current, xMin, yMin, xMax)
      else if (abs(curr.head - curr(2)) == abs(curr(1) - curr(3)))
        doXYUpdate(
          current,
          curr.head,
          curr(1),
          curr(2),
          curr(3)
        )
      else current
    }

    matrix.map(row => row.count(_ >= 2)).sum
  }
}
