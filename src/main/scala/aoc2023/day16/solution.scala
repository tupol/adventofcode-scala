package aoc2023.day16

import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.*
import scala.util.*

object solution extends App {

  type Board = IndexedSeq[IndexedSeq[Char]]
  val EmptyBoard: Board = IndexedSeq(IndexedSeq())

  def input: Board = Source
    .fromResource("aoc2023/day16/input.txt")
    .getLines()
    .map(_.trim)
    .filterNot(_.isEmpty)
    .toIndexedSeq
    .map(_.toCharArray)

  val sample: Board =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin
      .split("\n")
      .toIndexedSeq
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toCharArray)

  // Part 1 ============================================================================================

  /**
   * Light beam
   * @param x
   *   current horizontal position
   * @param y
   *   current vertical position
   * @param dx
   *   horizontal direction: -1 (moving left), 0 moving on the vertical axis, 1 moving right
   * @param dy
   *   vertical direction: -1 (moving up), 0 moving on the horizontal axis, 1 moving down
   */
  case class Beam(x: Int, y: Int, dx: Int, dy: Int) {
    require(abs(dx) + abs(dy) != 0)
  }

  object LightFlow {
    val optics = "\\/-|".toCharArray
  }

  case class LightFlow(board: Board, startBeam: Beam) extends Iterator[Seq[Beam]] {
    import LightFlow._

    val yMax = board.size
    val xMax = board.map(_.size).max

    val lightPath: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer.from(board.map(l => ArrayBuffer.from(l)))
    val hitOptics: Map[Char, ArrayBuffer[Beam]]   = optics.map(c => (c -> ArrayBuffer.empty[Beam])).toMap

    var current = Seq(startBeam)

    updateLightPath(current)

    def hasNext: Boolean = !current.isEmpty
    def next(): Seq[Beam] = {
      current = propagate(current)
      updateLightPath(current)
      current
    }

    def updateLightPath(beams: Seq[Beam]) =
      beams.map { beam =>
        val (bx, by) = (beam.x, beam.y)
        if (!optics.contains(lightPath(by)(bx)))
          (beam.dx, beam.dy) match {
            case (0, 1)  => lightPath.update(by, { lightPath(by).update(bx, 'v'); lightPath(by) })
            case (0, -1) => lightPath.update(by, { lightPath(by).update(bx, '^'); lightPath(by) })
            case (1, 0)  => lightPath.update(by, { lightPath(by).update(bx, '>'); lightPath(by) })
            case (-1, 0) => lightPath.update(by, { lightPath(by).update(bx, '<'); lightPath(by) })
          }
      }

    def offBoard(x: Int, y: Int): Boolean = x >= xMax || x < 0 || y >= yMax || y < 0

    def propagate(beams: Seq[Beam]): Seq[Beam] =
      beams.flatMap { beam =>
        val (bx, by) = (beam.x, beam.y)
        if (offBoard(bx, by))
          Nil
        else {
          val ci = board(by)(bx)
          val loop = hitOptics.get(ci) match {
            case Some(x) =>
              if (x.contains(beam)) true
              else { x.addOne(beam); false }
            case None => false
          }
          val newBeams = (ci, beam.dx, beam.dy) match {
            case ('/', 1, 0)   => Seq(beam.copy(x = bx, y = by - 1, dx = 0, dy = -1))
            case ('/', -1, 0)  => Seq(beam.copy(x = bx, y = by + 1, dx = 0, dy = +1))
            case ('/', 0, 1)   => Seq(beam.copy(x = bx - 1, y = by, dx = -1, dy = 0))
            case ('/', 0, -1)  => Seq(beam.copy(x = bx + 1, y = by, dx = +1, dy = 0))
            case ('\\', 1, 0)  => Seq(beam.copy(x = bx, y = by + 1, dx = 0, dy = +1))
            case ('\\', -1, 0) => Seq(beam.copy(x = bx, y = by - 1, dx = 0, dy = -1))
            case ('\\', 0, 1)  => Seq(beam.copy(x = bx + 1, y = by, dx = +1, dy = 0))
            case ('\\', 0, -1) => Seq(beam.copy(x = bx - 1, y = by, dx = -1, dy = 0))
            case ('-', _, 0)   => Seq(beam.copy(x = bx + beam.dx, y = by + beam.dy))
            case ('-', 0, _) =>
              Seq(
                beam.copy(x = bx - 1, y = by, dx = -1, dy = 0),
                beam.copy(x = bx + 1, y = by, dx = +1, dy = 0)
              )
            case ('|', 0, _) => Seq(beam.copy(x = bx + beam.dx, y = by + beam.dy))
            case ('|', _, 0) =>
              Seq(
                beam.copy(x = bx, y = by - 1, dx = 0, dy = -1),
                beam.copy(x = bx, y = by + 1, dx = 0, dy = +1)
              )
            case _ => Seq(beam.copy(x = bx + beam.dx, y = by + beam.dy))
          }
          newBeams.filterNot(b => offBoard(b.x, b.y) || loop)
        }
      }

    def printLightPath = {
      println(s"   0 1 2 3 4 5 6 7 8 9")
      lightPath.zipWithIndex
        .map(x => f"${x._2}%3d  ${x._1.mkString(" ")}  ${x._2}%3d")
        .foreach(println)
      println(s"   0 1 2 3 4 5 6 7 8 9")
    }
    def printBoard = println(board.map(_.mkString("")).mkString("\n"))
  }

  def coverage(start: Beam, lightBeams: Seq[Seq[Beam]]): Int =
    (lightBeams.flatten :+ start).map(x => (x.x, x.y)).toSet.size

  val start            = Beam(0, 0, 1, 0)
  val sampleLightFlow1 = LightFlow(sample, start)
  val sampleResult1    = coverage(start, sampleLightFlow1.toSeq)
  println(s"Sample 1: $sampleResult1")
  val lightFlow1 = LightFlow(input, start)
  val result1    = coverage(start, lightFlow1.toSeq)
  println(s"Part 1: $result1") // 6605

  // Part 2 ============================================================================================

  def startBeams(xMax: Int, yMax: Int) = ((0 until xMax).flatMap { x =>
    Seq(
      Beam(x = x, y = 0, dx = 0, dy = 1),
      Beam(x = x, y = yMax - 1, dx = 0, dy = -1)
    )
  } ++ (0 until yMax).flatMap { y =>
    Seq(
      Beam(x = 0, y = y, dx = 1, dy = 0),
      Beam(x = xMax - 1, y = y, dx = -1, dy = 0)
    )
  }).sortBy(b => (b.x, b.y))

  def maxCoverage(board: Board) = {
    val lf = LightFlow(board, Beam(0, 0, 0, 1))
    startBeams(lf.xMax, lf.yMax).map { beam =>
      val lightFlow = LightFlow(board, beam)
      val size      = coverage(beam, lightFlow.toSeq)
      (beam, size)
    }.map(_._2).max
  }

  val sampleResult2 = maxCoverage(sample)
  println(s"Sample 2: $sampleResult2") // 51

  val result2 = maxCoverage(input)
  println(s"Part 2: $result2") // 6766


}
