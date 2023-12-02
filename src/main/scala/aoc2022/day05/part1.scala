package aoc2022.day05

import scala.collection.immutable.Seq
import scala.io.*
import scala.util.{Failure, Try}

@main
def part1(): Unit = {

  val testStorage = Storage[String](3)
  testStorage.fill(0, Seq("Z", "N"))
  testStorage.fill(1, Seq("M", "C", "D"))
  testStorage.fill(2, Seq("P"))
  testStorage.show

  val commands = Seq(
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  ).map(MoveCommand(_))

  commands.foreach{ testStorage.move }

  testStorage.show

  println(testStorage.topContainers().mkString(" "))

  //    [H]         [H]         [V]
  //    [V]         [V] [J]     [F] [F]
  //    [S] [L]     [M] [B]     [L] [J]
  //    [C] [N] [B] [W] [D]     [D] [M]
  //[G] [L] [M] [S] [S] [C]     [T] [V]
  //[P] [B] [B] [P] [Q] [S] [L] [H] [B]
  //[N] [J] [D] [V] [C] [Q] [Q] [M] [P]
  //[R] [T] [T] [R] [G] [W] [F] [W] [L]
  // 1   2   3   4   5   6   7   8   9

  val mainStorage = Storage[Char](9)
  mainStorage.fill(0, "RNPG".toCharArray.toSeq)
  mainStorage.fill(1, "TJBLCSVH".toCharArray.toSeq)
  mainStorage.fill(2, "TDBMNL".toCharArray.toSeq)
  mainStorage.fill(3, "RVPSB".toCharArray.toSeq)
  mainStorage.fill(4, "GCQSWMVH".toCharArray.toSeq)
  mainStorage.fill(5, "WQSCDBJ".toCharArray.toSeq)
  mainStorage.fill(6, "FQL".toCharArray.toSeq)
  mainStorage.fill(7, "WMHTDLFV".toCharArray.toSeq)
  mainStorage.fill(8, "LPBVMJF".toCharArray.toSeq)
  mainStorage.show

  val mainCommands = Source.fromResource("day05/input.txt").getLines()
    .filter(_.startsWith("move"))
    .map(MoveCommand(_))

  mainCommands.foreach(mainStorage.move)

  mainStorage.show

  println(mainStorage.topContainers().mkString(""))

}