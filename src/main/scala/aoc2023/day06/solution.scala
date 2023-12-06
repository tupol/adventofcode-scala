package aoc2023.day06

import scala.io.Source
import scala.util.*

object solution extends App {

  case class Problem(time: Int, distance: Long) {
    def solve(): Int = (1 until time)
      .map{ t => Problem(t, (time - t) * t.toLong) }
      .filter(_.distance > distance)
      .size

  }

  val sample = Seq(
    Problem(7, 9),
    Problem(15, 40),
    Problem(30, 200)
  )

  val sampleResult = sample.map(_.solve()).reduce(_ * _)
  println(s"Sample 1: $sampleResult") // 288

  val input = Seq(
    Problem(62, 553),
    Problem(64, 1010),
    Problem(91, 1473),
    Problem(90, 1074)
  )

  val result1 = input.map(_.solve()).reduce(_ * _)
  println(s"Part 1: $result1") // 840336

  val sampleResult2 = Problem(71530, 940200).solve()
  println(s"Sample 2: $sampleResult2") //71503

  val result2 = Problem(62649190, 553101014731074L).solve()
  println(s"Part 2: $result2")

}
