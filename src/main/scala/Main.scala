
import Day1.{generateDay1, generateDay1b}
import Day2.{generateDay2, generateDay2b}
import Day3.{generateDay3, generateDay3b}
import Day4.{Board, Row, generateDay4a, generateDay4b, getBoards}
import Day5.{generateDay5, generateDay5b}
import Day6.{generateDay6, generateDay6b}
import Day8.{calculateDay8, calculateDay8b}
import Day9.{calculateDay9, calculateDay9b}
import cats.effect.{IO, Resource}

import java.io.File
import scala.sys.process.BasicIO.close
import cats.effect.*

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext
import cats.implicits.*


object Main extends cats.effect.IOApp {

  def run(args: List[String]): IO[ExitCode] =
    println(s"Hello world! ${java.io.File(".").getAbsolutePath}")
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
      a <- read("day4a.txt")
      b <- read("day4b.txt")
        _ = {
          val result = generateDay4a(a, b)
          val resultb = generateDay4b(a, b)
          println(s"day 4a = $result, 4b = $resultb")
        }
      _ <- read("day5.txt").map { list =>
        val res = generateDay5(list)
        val res2 = generateDay5b(list)
        println(s"day 5 = $res 5b = $res2")
      }
      _ <- read("day6.txt").map { list =>
        val res = generateDay6(list)
        val res2 = generateDay6b(list.toList)
        println(s"day 6 = $res 6b = $res2")
      }
      _ <- read("day7.txt").map { list =>
        val res = Day7.calculateDay7(list)
        val res2 = Day7.calculateDay7b(list)
        println(s"day 7 = $res 7b = $res2")
      }
      _ <- read("day8.txt").map { list =>
        val res = calculateDay8(list)
        val res2 = calculateDay8b(list)
        println(s"day 8 = $res 8b = $res2")
      }
      _ <- read("day9.txt").map { list =>
        val res = calculateDay9(list)
        val res2 = calculateDay9b(list)
        println(s"day 9 = $res 9b = $res2")
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
}