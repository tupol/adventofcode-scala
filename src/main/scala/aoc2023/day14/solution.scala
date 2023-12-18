package aoc2023.day14

import aoc2023.day13.solution.sampleResult
import aoc2023.day15.solution.{hashString, sample}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  type Board = Seq[Array[Char]]
  val EmptyBoard: Board = Seq()

  def input: Board = Source.fromResource("aoc2023/day14/input.txt").getLines()
    .map(_.trim).filterNot(_.isEmpty).toSeq.map(_.toCharArray)
  val sample: Board =
    """
      |O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty).map(_.toCharArray)
  val expected: Board =
    """
      |OOOO.#.O..
      |OO..#....#
      |OO..O##..O
      |O..#.OO...
      |........#.
      |..#....#.#
      |..O..#.O.O
      |..O.......
      |#....###..
      |#....#....
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty).map(_.toCharArray)


  val Ball = 'O'
  val Cube = '#'
  val Void = '.'

  def roll(src: Array[Char], dst: Array[Char]): (Array[Char], Array[Char]) = {
    val res = src.zip(dst).map{
      case (Ball, Void) => (Void, Ball)
      case (x, y) => (x, y)
    }
    (res.map(_._1), res.map(_._2))
  }

  def different(src: Array[Char], dst: Array[Char]): Boolean = src.zip(dst).map(_ != _).reduce(_ || _)

  def tiltBoard(board: Board): Board = {
    def tilt(board: Board): (Board, Boolean) =
      board.reverse.foldLeft((EmptyBoard, false)) { case ((acc, changes), cur) =>
        acc match {
          case Nil => (Array(cur), changes)
          case src +: t =>
            val (x, y) = roll(src, cur)
            (y +: x +: t, different(cur, y) || different(src, x) || changes)
        }
      }
    def loop(board: Board, done: Boolean = false): Board = {
      if(done) board
      else {
        val (newBoard, changes) = tilt(board)
        loop(newBoard, !changes)
      }
    }

    loop(board)
  }

  def loadOnNorthBeams(board: Board) =
    board.reverse.zipWithIndex
      .map{ case (line, i) => line.filter(_ == Ball).size * (i+1) }.sum

  sample.zip(tiltBoard(sample)).zip(expected)
    .map(x => (x._1._1.toSeq.mkString(""), x._1._2.toSeq.mkString(""), x._2.toSeq.mkString("")))
    .foreach{ (x, y ,z)  => println(s"|  $x  |  $y  |  $z  |") }

  // Part 1 ============================================================================================

  val sampleResult1 = loadOnNorthBeams(tiltBoard(sample))
  println(s"Sample 1: $sampleResult1")


  val result1 = loadOnNorthBeams(tiltBoard(input))
  println(s"Part 1: $result1")

  // Part 2 ============================================================================================

  def rotateClockwise(board: Board): Board =
    board.foldLeft(EmptyBoard) { case (acc, row) =>
      acc match {
        case Nil => row.map(x => Array(x))
        case acc => acc.zip(row).map((ax, c) => ax :+ c)
      }
    }

  def flip(board: Board): Board = board.map(_.reverse)
  def rotateCC(board: Board): Board =
    board.foldLeft(Seq[Array[Char]]()){ case (acc, row) =>
      acc match {
        case Nil => row.map(Array(_))
        case acc => acc.zip(row).map((ax, c) => ax :+ c)
      }
    }.reverse

  def rotateCW(board: Board): Board = flip(rotateCC(board)).reverse

  def printBoard(board: Board) = board.map(_.mkString("")).foreach(println)

  def cycle(board: Board): Board = {
    def loop(board: Board): Board = rotateCW(tiltBoard(board))
    (0 until 4).foldLeft(board)((acc, _) => loop(acc))
  }

  def cycles(board: Board, count: Int): Board =
    (1 until count).foldLeft(board){(acc, _) =>
      cycle(acc)
    }

  def loadCycles(board: Board, count: Int): Seq[(Int, Int, Board)] =
    (1 to count).foldLeft(Seq[(Int, Int, Board)]()) { (acc, i) =>
      acc match {
        case Nil =>
          val nb = cycle(board)
          (i, loadOnNorthBeams(nb), nb) +: acc
        case h +: _ =>
          val nb = cycle(h._3)
          (i, loadOnNorthBeams(nb), nb) +: acc
      }
    }

  // cycles(sample, 1000000000) should be 64

//  loadCycles(sample, 100000).filter(x => math.floorMod(x._1, 1000) == 0).map(x => (x._1, x._2)).foreach(println)
  loadCycles(sample, 100).map(x => (x._1, x._2)).foreach(println)
//  println(loadOnNorthBeams(cycles(sample, 1000000000)))
}
