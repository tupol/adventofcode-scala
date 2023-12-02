package aoc2022.day06

import scala.annotation.tailrec
import scala.io.*

@main
def part2(): Unit = {

  println(firstMarker(test1, 14).map(_._2 == 19))
  println(firstMarker(test2, 14).map(_._2 == 23))
  println(firstMarker(test3, 14).map(_._2 == 23))
  println(firstMarker(test4, 14).map(_._2 == 29))
  println(firstMarker(test5, 14).map(_._2 == 26))


  val main = Source.fromResource("day06/input.txt")
  println(firstMarker(main, 14))

}