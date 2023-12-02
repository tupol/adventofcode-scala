package aoc2022.day03

import scala.io.*

@main
def part1(): Unit = {

  def input(path: String): Seq[(String, String)] = Source.fromResource(path).getLines()
    .map(_.trim).filterNot(_.isEmpty)
    .map(line => line.splitAt(line.size/2))
    .toSeq

  def result(input: Seq[(String, String)]): Int =
    input.map{ case (l, r) => l.intersect(r).map(alphaMap.getOrElse(_, 0)).toSet.sum }.sum

  val testInput = input("day03/input1_test.txt")
  val testResult = result(testInput)
  println(testResult == 157)

  val mainInput  = input("day03/input1.txt")
  val mainResult = result(mainInput)
  println(mainResult)

}