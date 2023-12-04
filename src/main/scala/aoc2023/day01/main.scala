package aoc2023.day01

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object main extends App {

  val digits = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    .zipWithIndex.map{ case (str, int) => (str, int + 1)}

  val mightBeDigit = digits.map(_._1.head).toSet


  def correction(input: String) = {
    val correctedStr = digits.foldLeft(input)((acc, digit) => acc.replaceAll(digit._1, s"${digit._2}"))
    val inputDigits: Seq[Int] = correctedStr.filter(_.isDigit).map(_.toString.toInt)
    val a = inputDigits.headOption.getOrElse(0)
    val b = inputDigits.lastOption.getOrElse(0)
    a * 10 + b
  }

  def correction2(input: String) = {

    val dx = (0 until input.size).map { i =>
      val c = input(i)
      if (c.isDigit)
        Some(c.toString.toInt)
      else if (mightBeDigit.contains(c)) {
        digits.map { d =>
          val t = i + d._1.size
          val res = if (input.isDefinedAt(t-1)) {
            val x = input.subSequence(i, t).toString.toLowerCase
            if (x == d._1) Some(d._2) else None
          } else None
          res
        }.collect{ case Some(x) => x}.headOption
      } else
        None
    }

    val inputDigits: Seq[Int] = dx.collect{ case Some(x) => x }
    val a = inputDigits.headOption.getOrElse(0)
    val b = inputDigits.lastOption.getOrElse(0)
    a * 10 + b
  }

  // Check if there is a match until the first match, not all digits
  def correction2Optimized(input: String) = {

    val dx = (0 until input.size).map { i =>
      val c = input(i)
      if (c.isDigit) 
        Some(c.toString.toInt)
      else if (mightBeDigit.contains(c))
        val dit = digits.iterator
        var res1: Option[Int] = None
        while (dit.hasNext && res1.isEmpty) {
          val d = dit.next()
          val t = i + d._1.size
          if (input.isDefinedAt(t - 1)) {
            val x = input.subSequence(i, t).toString.toLowerCase
            if (x == d._1) {
              res1 = Some(d._2)
            }
          }
        }
        res1
      else
        None
    }
    val inputDigits: Seq[Int] = dx.collect { case Some(x) => x }
    val a = inputDigits.headOption.getOrElse(0)
    val b = inputDigits.lastOption.getOrElse(0)
    a * 10 + b
  }

  // The concept of this one is to move ahead the cursor, but it the is a problem when you have digits like
  // oneight which in this case leads to 1 but it should be 18
  // Good idea, but not in this case :)
  def correction2Optimized2Far(input: String) = {

    var i = 0
    val collector = ArrayBuffer[Int]()

    while(i < input.size) {
      val c = input(i)
      if (c.isDigit) {
        collector.addOne(c.toString.toInt)
        i += 1
      } else if (mightBeDigit.contains(c)) {
        val dit = digits.iterator
        var stop = false
        while (dit.hasNext && !stop) {
          val d = dit.next()
          val t = i + d._1.size
          if (input.isDefinedAt(t - 1)) {
            val x = input.subSequence(i, t).toString.toLowerCase
            if (x == d._1) {
              stop = true
              collector.addOne(d._2)
              i = i + d._1.size - 1
            }
          }
        }
        i += 1
      } else
        i += 1
    }

    val a = collector.headOption.getOrElse(0)
    val b = collector.lastOption.getOrElse(0)
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

  val resultSample = sampleInput.map(correction2Optimized).reduce(_ + _)
  println(resultSample)

  val input  = Source.fromResource("aoc2023/day01/input.txt").getLines()
  val result = input.map(correction2Optimized).sum
  println(result)
}
