import cats.effect.{IO, Resource}

import java.io.File
import scala.sys.process.BasicIO.close
import cats.effect._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import cats.implicits._


object Main extends cats.effect.IOApp {

  def run(args: List[String]): IO[ExitCode] =
    println("Hello world!")
    for {
      x <- read("day1.txt").map { l =>
        val day1 = generateDay1(l.map(_.toInt))
        val day2 = generateDay1b(l.map(_.toInt))
        println(s"$day1, $day2")
      }
      _ <- read("day2.txt").map { l =>
        val res = generateDay2(l)
        val res2 = generateDay2b(l)
        println(s"$res $res2")
      }
      _ <- read("day3.txt").map { l =>
        val res = generateDay3(l)
        val res2 = generateDay3b(l)
        println(s"$res $res2")
      }
    } yield ExitCode.Success



  def read(name: String): IO[Seq[String]] =
    val source = scala.io.Source.fromResource(name)

    IO(source).bracket { in =>
      IO {
        in.getLines().toSeq
      }
    } { in =>
      IO(in.close())
    }

  def generateDay1(list: Seq[Int]): Int =
    list.sliding(2).foldLeft(0){ (acc, curr) =>
      if (curr(1) > curr.head) acc + 1
      else acc
    }

  def generateDay1b(list: Seq[Int]): Int =
    list.sliding(4).foldLeft(0){ (acc, curr) =>
      val prev = curr.take(3).sum
      val next = curr.drop(1).sum
      if (next > prev) acc + 1
      else acc
    }

  def generateDay2(list: Seq[String]): Int = {
    val (horizontal, depty) = list.foldLeft(0, 0) { (acc, s) =>
      val line = s.split(" ")
      val cmd = line.head.trim
      val value = line(1).toInt
      cmd match {
        case "up" => (acc._1, acc._2 - value)
        case "forward" => (acc._1 + value, acc._2)
        case "down" => (acc._1, acc._2 + value)
        case _ => throw new RuntimeException("wrong input")
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
          case "up" => (acc._1, acc._2, acc._3 - value)
          case "forward" => (acc._1 + value, acc._2 + (acc._3 * value), acc._3)
          case "down" => (acc._1, acc._2, acc._3 + value)
          case _ => throw new RuntimeException("wrong input")
        }
      }
      horizontal * depth
    }


  def generateDay3(list: Seq[String]): Int = {
    val onesCount: Seq[Int] = list.foldLeft(Seq.fill(12)(0)) { (curr, row) =>
      row.zipWithIndex.map {  case (chr, idx) =>
        curr(idx) + Integer.parseInt(chr.toString)
      }
    }
    val binaryGammaStr = onesCount.map(x => if (x > list.size / 2) "1" else "0").mkString
    val binaryEpsilonStr = onesCount.map(x => if (x < list.size / 2) "1" else "0").mkString
    val gamma = Integer.parseInt(binaryGammaStr, 2)
    val epsilon = Integer.parseInt(binaryEpsilonStr, 2)

    gamma * epsilon
  }

  def generateDay3b(list: Seq[String]): Int = {

    def filter(idx: Int, filterOn: Char,  list: Seq[String]): String = {
      if (list.size == 1 || idx == 12) list.head
      else {
        val (onesList, zerosList) = list.partition { str => str(idx) == filterOn }
        val biggestList = if (zerosList.size >= onesList.size)  zerosList else onesList
        filter(idx + 1, filterOn, biggestList)
      }
    }

    val binaryO2Rating = filter(0, '1', list)
    val o2Rating = Integer.parseInt(binaryO2Rating, 2)

    val binaryScrubberRating = filter(0, '0', list)
    val scrubberRating = Integer.parseInt(binaryScrubberRating, 2)

    o2Rating * scrubberRating
  }
}