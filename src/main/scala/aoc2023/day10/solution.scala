package aoc2023.day10

import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*
import aoc2023.day07.solution.Part1Rules
import aoc2023.day10.solution.problem

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  val SE = 'F'
  val NE = 'L'
  val NW = 'J'
  val SW = '7'
  val NS = '|'
  val EW = '-'
  val OO = '.'
  val XX = 'S'
  val Horizontal = EW
  val Vertical = NS

  val sample1: IndexedSeq[IndexedSeq[Char]] =
    """
      |..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...
      |""".stripMargin.split("\n").map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

  def input: IndexedSeq[IndexedSeq[Char]] = Source.fromResource("aoc2023/day10/input.txt").getLines().map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

  case class Pos(y: Int, x: Int) {
    def moveNorth(): Pos = this.copy(y = y - 1)
    def moveSouth(): Pos = this.copy(y = y + 1)
    def moveEast(): Pos = this.copy(x = x + 1)
    def moveWest(): Pos = this.copy(x = x - 1)

    override def toString: String = s"($y, $x)"
  }
  case class Problem(matrix: IndexedSeq[IndexedSeq[Char]], pos: Pos, prevPos: Pos, distance: Int) {
    require(isPossible(pos), s"Position $pos does not fit into the matrix")
    val item = matrix(pos.y)(pos.x)
    private def isPossible(pos: Pos): Boolean = pos.y >= 0 && pos.y < matrix.size && pos.x >= 0 && pos.x < matrix(pos.y).size
    def nextPos(): Seq[Pos] = item match {
      case OO => Nil
      case NE => Seq(pos.moveNorth(), pos.moveEast()).filter(isPossible)
      case SE => Seq(pos.moveSouth(), pos.moveEast()).filter(isPossible)
      case NW => Seq(pos.moveNorth(), pos.moveWest()).filter(isPossible)
      case SW => Seq(pos.moveSouth(), pos.moveWest()).filter(isPossible)
      case NS => Seq(pos.moveNorth(), pos.moveSouth()).filter(isPossible)
      case EW => Seq(pos.moveEast(), pos.moveWest()).filter(isPossible)
      case XX => Seq(pos.moveNorth(), pos.moveSouth(), pos.moveEast(), pos.moveWest()).filter(isPossible)
    }
    def nextProb(): Seq[Problem] = nextPos().filterNot(_ == prevPos).map(p => this.copy(pos = p, prevPos = pos, distance = this.distance + 1)).filter(_.nextPos().contains(this.pos))

    override def toString: String = s"$item $pos $distance"
  }
  object Problem {
    def findStart(matrix: IndexedSeq[IndexedSeq[Char]]): Pos = {
      val res = matrix.zipWithIndex.map { case (line, y) =>
        (y, line.zipWithIndex.filter(_._1 == XX).headOption)
      }.filter(_._2.isDefined).head
      Pos(y = res._1, x = res._2.get._2)
    }
    def apply(matrix: IndexedSeq[IndexedSeq[Char]]): Problem = {
      val start = findStart(matrix)
      Problem( matrix, start, start, 0)
    }
  }

  case class LoopFinder(problem: Problem) extends Iterator[Problem] {
    var current: Option[Problem] = None
//    println(s"problem = $problem  | current = $current")
    def hasNext: Boolean = {
      val hn = current.map(_.pos != problem.pos).getOrElse(true)
//      println(s"problem = $problem  |  current = $current  |  hasNext = $hn")
      hn
    }

    def next(): Problem = {
      val next = current.getOrElse(problem).nextProb().head
      current = Some(next)
      next
    }
  }

  val sampleResult1 = LoopFinder(Problem(sample1)).toSeq.size / 2
  println(s"Sample 1: $sampleResult1") // 8

  val problem = Problem(input)
  val loop: Seq[Pos] = LoopFinder(problem).toSeq.map(_.pos)

  val result1 = loop.size / 2
  println(s"Part 1: $result1") // 6725
  
  // TODO: PART 2 :)

  case class LoopContents(problem: Problem) {
    val corners = Seq(NE, NW, SE, SW)
    def canInlineHorizontally(a: Char, b: Char): Boolean = (a == NE && b == SW) || (a == SE && b == NW)
    def canInlineVertically(a: Char, b: Char): Boolean = (a == NW && b == SE) || (a == SW && b == NE)

    lazy val loop: Seq[Pos] = LoopFinder(problem).toSeq.map(_.pos)
    def item(pos: Pos): Char = problem.matrix(pos.y)(pos.x)
    def isOnLoop(pos: Pos) = loop.filter(_ == pos).size > 0
    def isCorner(pos: Pos) = corners.contains(item(pos))

    def isVertical(c: Char): Boolean = c == NS
    def isVertical(pos: Pos): Boolean = isVertical(item(pos))

    def isHorizontal(c: Char): Boolean = c == EW
    def isHorizontal(pos: Pos): Boolean = isHorizontal(item(pos))
    def loopItemsOnRow(y: Int) = {
      println(s"loopItemsOnRow = ${
        loop
          .filter(_.y == y)
          .filterNot(isHorizontal)
          .map(p => (item(p), p))
      }")
      loop
        .filter(_.y == y)
        .filterNot(isHorizontal)
        .map(p => (item(p), p))
        .foldLeft(Seq[(Char, Pos)]())((acc, p) =>
          acc match {
            case x +: xs if canInlineVertically(x._1, p._1) => (Vertical, p._2) +: xs
            case Nil => acc
          }
        )
        .filter(x => isVertical(x._1))
    }
    def loopItemsOnCol(x: Int) = {
//      println(s"loopItemsOnCol = ${
//        loop.filter(_.x == x)
//          .filterNot(isVertical)
//          .map(p => (item(p), p))}")
      loop.filter(_.x == x)
        .filterNot(isVertical)
        .map(p => (item(p), p))
        .foldLeft(Seq[(Char, Pos)]())((acc, p) =>
          acc match {
            case Nil => acc
            case x +: xs if canInlineHorizontally(x._1, p._1) => (Horizontal, p._2) +: xs
            case _ => p +: acc

          }
        ).filter(x => isHorizontal(x._1))
    }
    def isInLoop(pos: Pos) = {
      val res = !isOnLoop(pos) &&
        ( math.floorMod(loopItemsOnRow(pos.y).filter(_._2.x < pos.x).size, 2) == 1 ||
          math.floorMod(loopItemsOnCol(pos.x).filter(_._2.y < pos.y).size, 2) == 1 )
//      println(s"# $pos  $res ----------------------------------------")
//      println(s"  isOnLoop(pos) = ${isOnLoop(pos)}")
//      println(s"  loopItemsOnRow(pos.y).filter(_.x < pos.x).size = ${loopItemsOnRow(pos.y).filter(_._2.x < pos.x)}")
//      println(s"  loopItemsOnCol(pos.x).filter(_.y < pos.y).size = ${loopItemsOnCol(pos.x).filter(_._2.y < pos.y)}")
      res
    }
    def printLoop: Unit =
      problem.matrix.zipWithIndex.map{ case (line, y) =>
        line.zipWithIndex.map { case (c, x) =>
          val pos = Pos(x = x, y = y)
          if(isOnLoop(pos)) c
          else if(isInLoop(pos)) '*'
          else ' '
        }.mkString("")
      }.foreach(println)

    def contents(): Seq[Pos] = for {
      ys <- problem.matrix.zipWithIndex
      y = ys._2
      xs <- ys._1.zipWithIndex
      x = xs._2
      pos = Pos(x = x, y = y)
      if(isInLoop(pos))
    } yield pos
  }

  val sample21 =
    """
      |...........
      |.S-------7.
      |.|F-----7|.
      |.||.....||.
      |.||.....||.
      |.|L-7.F-J|.
      |.|..|.|..|.
      |.L--J.L--J.
      |...........
      |""".stripMargin.split("\n").map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

  val problem21 = Problem(sample21)
  val loopContents21 = LoopContents(problem21)
  val resultSample21 = loopContents21.contents()
  println(s"Sample 21: ${resultSample21.size}")
  println(s"Sample 21: ${resultSample21}")
  loopContents21.problem.matrix.zipWithIndex.foreach { case (line, y) =>
    val ior = loopContents21.loopItemsOnRow(y)
    println(f"$y%2d  |  $line  |  $ior")
  }

  val sample22 =
    """
      |..........
      |.S------7.
      |.|F----7|.
      |.||OOOO||.
      |.||OOOO||.
      |.|L-7F-J|.
      |.|II||II|.
      |.L--JL--J.
      |..........
      |""".stripMargin.split("\n").map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

//  val problem22 = Problem(sample22)
//  val resultSample22 = LoopContents(problem22).contents()
//  println(s"Sample 22: ${resultSample22.size}")
//  println(s"Sample 22: ${resultSample22}")


  val sample23 =
    """
      |.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...
      |""".stripMargin.split("\n").map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

  //    01234567890123456789
  // 0  OF----7F7F7F7F-7OOOO
  // 1  O|F--7||||||||FJOOOO
  // 3  O||OFJ||||||||L7OOOO
  // 4  FJL7L7LJLJ||LJIL-7OO
  // 5  L--JOL7IIILJS7F-7L7O
  // 6  OOOOF-JIIF7FJ|L7L7L7
  // 7  OOOOL7IF7||L7|IL7L7|
  // 8  OOOOO|FJLJ|FJ|F7|OLJ
  // 9  OOOOFJL-7O||O||||OOO
  //10  OOOOL---JOLJOLJLJOOO
  //    01234567890123456789
  // (3, 14), (4, 7), (4, 8), (4, 9), (5, 7), (5, 8), (6, 6), (6, 14)

//  val problem23 = Problem(sample23)
//  val solver23 = LoopContents(problem23)
////  val resultSample23 = solver23.contents()
////  println(s"Sample 23: ${resultSample23}")
////  solver23.printLoop
//  println("===================================================================================================================")
//  val expected = Seq(Pos(3, 14), Pos(4, 7), Pos(4, 8), Pos(4, 9), Pos(5, 7), Pos(5, 8), Pos(6, 6), Pos(6, 14))
//  expected.foreach(x => println(s"$x ${solver23.isInLoop(x)}"))

}
