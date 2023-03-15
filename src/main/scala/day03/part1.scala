package day03

import scala.io.*

@main
def part1(): Unit = {

  val lower = "abcdefghijklmnopqrstuvwxyz".zipWithIndex.map{ case (l, i) => (l, i + 1) }
  val upper = lower.map{ case (l, i) => (l.toUpper, i) }

  def input(path: String): Seq[(String, String)] = Source.fromResource(path).getLines()
    .map(_.trim).filterNot(_.isEmpty)
    .map(line => line.splitAt(line.size/2))
    .toSeq

  val testInput = input("day03/input_test.txt")
  testInput.map{ case (l, r) => l.intersect(r) }.foreach(println)

}