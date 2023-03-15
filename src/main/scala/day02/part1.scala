package day02

import scala.io._

@main
def part1(): Unit = {

  def input(path: String): Seq[(Choice, Choice)] = Source.fromResource(path).getLines()
    .map { line =>
      val res = line.trim.split(" ").map(Choice.fromString)
      (res.head, res.tail.head)
    }.toSeq

  val testInput = input("day02/input_test.txt")
  println(testInput.map { case (a, b) => scoreB(a, b) }.sum == 15)

  val mainInput = input("day02/input.txt")

  println(mainInput.map{case (a, b) => scoreB(a, b)}.sum)

}