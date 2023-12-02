package aoc2023.day01

import scala.io.Source

object part2 extends App {

  val digits = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    .zipWithIndex.map{ case (str, int) => (str, int + 1)}

  def correction2(input: String) = {
    val mightBeDigit = digits.map(_._1.head).toSet

    val dx = (0 until input.size).map { i =>
      val c = input(i)
//      print(s"# $c")
      if (c.isDigit) {
//        println(s" isDigit")
        Some(c.toString.toInt)
      } else if (mightBeDigit.contains(c)) {
//        println(s" mightBeDigit")
        digits.map { d =>
          val t = i + d._1.size
          val res = if (input.isDefinedAt(t-1)) {
            val x = input.subSequence(i, t).toString.toLowerCase
//            print(s"$x ")
            if (x == d._1) Some(d._2) else None
          } else None
//          println
          res
        }.collect{ case Some(x) => x}.headOption
      } else {
//        println(s" notDigit")
        None
      }
    }
    val inputDigits: Seq[Int] = dx.collect{ case Some(x) => x }
    val a = inputDigits.headOption.getOrElse(0)
    val b = inputDigits.lastOption.getOrElse(0)
    a * 10 + b
  }

  def correction(input: String) = {
    val correctedStr = digits.foldLeft(input)((acc, digit) => acc.replaceAll(digit._1, s"${digit._2}"))
    val inputDigits: Seq[Int] = correctedStr.filter(_.isDigit).map(_.toString.toInt)
    val a = inputDigits.headOption.getOrElse(0)
    val b = inputDigits.lastOption.getOrElse(0)
    a * 10 + b
  }

  val sampleInput =
    """
      |two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen
      |""".stripMargin.split("\n")

  val resultSample = sampleInput.map(correction2).reduce(_ + _)
  println(resultSample)

  val input  = Source.fromResource("aoc2023/day01/input1.txt").getLines()
  val result = input.map(correction2).reduce(_ + _)
  println(result)
}
