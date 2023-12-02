package aoc2022.day06

import scala.annotation.tailrec
import scala.io.*

@main
def part1(): Unit = {

  println(firstMarker(test1, 4).map(_._2 == 7))
  println(firstMarker(test2, 4).map(_._2 == 5))
  println(firstMarker(test3, 4).map(_._2 == 6))
  println(firstMarker(test4, 4).map(_._2 == 10))
  println(firstMarker(test5, 4).map(_._2 == 11))

  val main = Source.fromResource("day06/input.txt")
  println(firstMarker(main, 4))

}