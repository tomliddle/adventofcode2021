
import Day4.{Board, Row, generateDay4a, generateDay4b, getBoards}
import Day5.{generateDay5, generateDay5b}
import Day6.{generateDay6, generateDay6b}
import cats.effect.{IO, Resource}

import java.io.File
import scala.sys.process.BasicIO.close
import cats.effect.*

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext
import cats.implicits.*


object Main extends cats.effect.IOApp {

  def run(args: List[String]): IO[ExitCode] =
    println("Hello world!")
    for {
      x <- read("day1.txt").map { l =>
      //  val day1 = generateDay1(l.map(_.toInt))
       // val day2 = generateDay1b(l.map(_.toInt))
       // println(s"$day1, $day2")
      }
      _ <- read("day2.txt").map { l =>
       // val res = generateDay2(l)
       // val res2 = generateDay2b(l)
       // println(s"$res $res2")
      }
      _ <- read("day3.txt").map { l =>
       // val res = generateDay3(l)
       // val res2 = generateDay3b(l)
       // println(s"$res $res2")
      }
      a <- read("day4a.txt")
      b <- read("day4b.txt")
        x = {
          val result = generateDay4a(a.head.split(",").map(no => Integer.parseInt(no)), getBoards(b))
          val resultb = generateDay4b(a.head.split(",").map(no => Integer.parseInt(no)), getBoards(b))
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