package day03

import scala.io.*

@main
def part2(): Unit = {

  def input(path: String) = Source.fromResource(path).getLines()
    .map(_.trim).filterNot(_.isEmpty)
    .toSeq

  def result(input: Seq[String]) = input
    .sliding(3, 3)
    .map(team => team.tail.foldLeft(team.head)((acc, elf) => acc.intersect(elf)))
    .mkString("")
    .toCharArray.map(c => priority(c))
    .sum


  val testResult = result(input("day03/input2_test.txt"))
  println(testResult == 70)

  val mainResult = result(input("day03/input2.txt"))
  println(mainResult)
}