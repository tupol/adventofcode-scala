package aoc2023.day03

import aoc2023.day02.main.Game


import scala.io.Source

object main extends App {

  trait Text {
    val value: String
    def from: Int
    def to: Int = from + value.size - 1
    override def toString: String = s"$value [$from, $to]"
  }

  case class Digits(value: String, from: Int) extends Text {
    val number = value.toInt
    def append(value: String) = Digits(s"${this.value}$value", this.from)
  }
  case class DigitsWithId(id: Int, digits: Digits) {
    override def toString: String = f"[$id%4d] ${digits.number}%5d"
  }
  case class Symbols(value: String, from: Int) extends Text {
    def append(value: String) = Symbols(s"${this.value}$value", this.from)
  }

  def checkNumberSymbolProximity(number: Digits, symbols: Text): Boolean = {
    //    println(s"number = $number  symbols = $symbols")
    number != symbols &&
      ((symbols.from >= number.from - 1 && symbols.from <= number.to + 1) ||
      (symbols.to >= number.from - 1 && symbols.to <= number.to + 1))
  }

  def extractDigits(input: String) = {
    input.zipWithIndex
      .filter(_._1.isDigit)
      .foldLeft(Seq.empty[Digits]) { case (acc, (d, i)) =>
        val prev = acc.lastOption
        prev match {
          case Some(dx) if i == dx.to + 1 =>
            val newDigit = dx.append(d.toString)
            acc.dropRight(1) :+ newDigit
          case _ =>
            acc :+ Digits(d.toString, i)
        }
      }
  }

  def extractSymbols(input: String) = {
    input.zipWithIndex
      .filterNot(c => c._1.isDigit || c._1 == '.')
      .foldLeft(Seq.empty[Symbols]) { case (acc, (d, i)) =>
        val prev = acc.lastOption
        prev match {
          case Some(dx) if i == dx.to + 1 =>
            val newDigit = dx.append(d.toString)
            acc.dropRight(1) :+ newDigit
          case _ =>
            acc :+ Symbols(d.toString, i)
        }
      }
  }

  def findEligibleDigits(digits: Seq[DigitsWithId], symbols: Seq[Text]): Seq[DigitsWithId] =
    digits.filter{ di =>
      symbols
        .map(s => checkNumberSymbolProximity(di.digits, s))
        .reduceOption(_ || _)
        .getOrElse(false)
    }


  def findMissingParts(input: Iterator[String]): Iterator[DigitsWithId] = {
    input.zipWithIndex.sliding(2, 1).flatMap { case group =>
//      println("------------------------------------------------")
//      group.foreach(println)
      val dix = group.flatMap((l, i) => extractDigits(l).map(DigitsWithId(i, _)))
      val six = group.map(_._1).flatMap(extractSymbols) ++ group.map(_._1).flatMap(extractDigits)
      val res = findEligibleDigits(dix, six)
//      println(res)
      res
    }
  }

  def addMissingParts(parts: Set[DigitsWithId]): Long =
    parts.toSeq.map(_.digits.number.toLong).sum

  def printResults(results: Set[DigitsWithId]) =
    results.groupBy(_.id).toSeq
      .sortBy(_._1)
      .foreach { case (id, rx) =>
        println(f"[$id%4d] (${rx.toSeq.map(_.digits.number).sum}) ${rx.toSeq.sortBy(_.digits.from).map(x => f"${x.digits.number}%3d").mkString("  ")}")
      }

  val sampleInput: Seq[String] =
    """
      |467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)
  val sampleResult = findMissingParts(sampleInput.iterator).toSet
  printResults(sampleResult)
  println(addMissingParts(sampleResult))

  val input = Source.fromResource("aoc2023/day03/input1.txt").getLines()
  val result = findMissingParts(input).toSet
  printResults(result)
  println(addMissingParts(result))
}
