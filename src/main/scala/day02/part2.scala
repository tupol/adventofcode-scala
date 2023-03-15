package day02

import scala.io.*

@main
def part2(): Unit = {

  def strategy(b: Outcome): Choice => Choice = {

    def win(a: Choice): Choice =
      a match
        case Choice.Rock => Choice.Paper
        case Choice.Paper => Choice.Scissor
        case Choice.Scissor => Choice.Rock

    def loose(a: Choice): Choice =
      a match
        case Choice.Rock => Choice.Scissor
        case Choice.Paper => Choice.Rock
        case Choice.Scissor => Choice.Paper

    def draw(a: Choice): Choice = a

    b match
      case Outcome.Win => win
      case Outcome.Loose => loose
      case Outcome.Draw => draw
  }

  def input(path: String): Seq[(Choice, Outcome)] = Source.fromResource(path).getLines()
    .map{ line =>
      val res = line.trim.split(" ")
      (Choice.fromString(res.head), Outcome.fromString(res.tail.head))
    }.toSeq

  def result(input: Seq[(Choice, Outcome)]): Int =
    input.map { case (a, o) =>
      val b = strategy(o)(a)
      scoreB(a, b)
    }.sum

  val testResult = result(input("day02/input_test.txt"))
  println(testResult == 12)

  val mainResult = result(input("day02/input.txt"))
  println(mainResult)
}