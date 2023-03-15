package day01

import scala.io._

@main
def main(): Unit = {

  def parseInput(input: Iterator[String]): Seq[Seq[Int]] = {
    val (accu, rest) = input
      .foldLeft((Seq[Seq[Int]](), Seq[Int]()))( (accu, in) =>
        (accu._2.isEmpty, in.isEmpty) match 
          case (false, true) => (accu._1 :+ accu._2, Seq[Int]())
          case (_, false)    => (accu._1, accu._2 :+ in.toInt)
          case (_, _)        => (accu._1, accu._2)
    )
    if(rest.isEmpty) accu else accu :+ rest
  }

  val input  = Source.fromResource("day01/input.txt").getLines()
  println(parseInput(input).map(_.sum).sortBy(-_).take(3).sum)
}