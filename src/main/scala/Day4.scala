object Day4 {

  case class Row(numbers: Seq[Int]) {
    def rowWin(nos: Set[Int]): Boolean = {
      numbers.forall(n => nos.contains(n))
    }
  }

  case class Board(rows: Seq[Row]) {
    private val vertialRows = rows.zipWithIndex.map { case (_, idx) =>
      val verticalNos = rows.map(row => row.numbers(idx))
      Row(verticalNos)
    }

    def isWin(nos: Set[Int]): Boolean = {
      val rowsWin = rows.exists(row => row.rowWin(nos))
      lazy val verticalWin =
        vertialRows.exists(row => row.rowWin(nos))
      rowsWin || verticalWin
    }

    def unmarkedNumbers(nos: Set[Int]): Set[Int] = {
      rows.flatMap(_.numbers).toSet.removedAll(nos)
    }
  }

  def getBoards(
      list: Seq[String],
      boards: Seq[Board] = Seq.empty[Board]
  ): Seq[Board] = {
    if (list.isEmpty) boards
    else {
      val rows: Seq[Row] = list.take(5).map { row =>
        Row(row.trim.split("\\s+").toSeq.map { no =>
          Integer.parseInt(no)
        })
      }
      getBoards(list.drop(6), Board(rows) +: boards)
    }
  }

  def calculateResult(brd: Board, drawn: Seq[Int]) = {
    (brd.unmarkedNumbers(drawn.toSet), drawn.head)
  }

  def generateDay4a(lista: Seq[String], listb: Seq[String]): Int = {
    do4a(
      lista.head.split(",").map(no => Integer.parseInt(no)),
      getBoards(listb)
    )
  }

  def do4a(numberList: Seq[Int], boards: Seq[Board]): Int = {

    def calc(drawn: Seq[Int], remaining: Seq[Int]): (Set[Int], Int) = {
      val brd: Option[Board] = boards.find {
        _.isWin(drawn.toSet)
      }

      brd match {
        case Some(l) => calculateResult(l, drawn)
        case None    => calc(remaining.head +: drawn, remaining.tail)
      }
    }

    val res = calc(numberList.take(5).reverse, numberList.drop(5))
    res._1.sum * res._2
  }

  def generateDay4b(lista: Seq[String], listb: Seq[String]): Int = {
    do4b(
      lista.head.split(",").map(no => Integer.parseInt(no)),
      getBoards(listb)
    )
  }

  def do4b(numberList: Seq[Int], boards: Seq[Board]): Int = {
    def calc(
        drawn: Seq[Int],
        remaining: Seq[Int],
        remainingBrds: Seq[Board],
        winningBrds: Seq[Board]
    ): (Set[Int], Int) = {

      val (winners, nonWinners) = remainingBrds.partition { brd =>
        brd.isWin(drawn.toSet)
      }

      if (nonWinners.isEmpty)
        calculateResult(winners.last, drawn)
      else calc(remaining.head +: drawn, remaining.tail, nonWinners, winners)
    }

    val res =
      calc(numberList.take(5).reverse, numberList.drop(5), boards, Seq.empty)
    res._1.sum * res._2
  }
}
