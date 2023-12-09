package aoc2023.day09

import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*
import aoc2023.day07.solution.Part1Rules

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  def diffElements(input: Seq[Int], result: Seq[Int] = Seq()): Seq[Int] = input match {
    case a +: b +: rest => diffElements(b +: rest, (b - a) +: result )
    case _ => result.reverse
  }

  def diffLines(input: Seq[Int], results: Seq[Seq[Int]] = Seq()): Seq[Seq[Int]] = input match {
    case _ if input.forall(_ == 0) => results
    case _ =>
      val diffs = diffElements(input)
      diffLines(diffs, diffs +: results)
  }

  def predictNext(lines: Seq[Seq[Int]], result: Int = 0): Int = lines match {
    case Nil => result
    case x +: rest => predictNext(rest, x.lastOption.getOrElse(0) + result)
  }

  def predictPrev(lines: Seq[Seq[Int]], result: Int = 0): Int = lines match {
    case Nil => result
    case x +: rest => predictPrev(rest, x.headOption.getOrElse(0) - result)
  }

  def predict1(input: Seq[Int]): Int =
    predictNext(diffLines(input)) + input.last

  def predict2(input: Seq[Int]): Int =
     input.head - predictPrev(diffLines(input))

  def parseLine(input: String): Seq[Int] = input.split(" ").map(_.trim.toInt)

  val sample =
    """
      |0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

  def input = Source.fromResource("aoc2023/day09/input.txt").getLines()

  val sampleResult1 = sample.map(parseLine).map(predict1).sum
  println(s"Sample 1: $sampleResult1") // 68

  val result1 = input.map(parseLine).map(predict1).sum
  println(s"Part 1: $result1") // 1789635132

  val sampleResult2 = sample.map(parseLine).map(predict2).sum
  println(s"Sample 2: $sampleResult2") // 2

  val result2 = input.map(parseLine).map(predict2).sum
  println(s"Part 2: $result2") // 913


}
