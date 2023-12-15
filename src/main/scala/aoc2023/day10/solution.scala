package aoc2023.day10

import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*
import aoc2023.day07.solution.Part1Rules
import aoc2023.day10.solution.LoopContents.isVertical
import aoc2023.day10.solution.problem

import scala.collection.immutable.Seq
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

    import LoopContents._

    lazy val loop: Seq[(Char, Pos)] = {
      val originalLoop = LoopFinder(problem).toSeq.map(x => (x.item, x.pos))
      val loopStart = (LoopContents.findLoopStart(originalLoop), originalLoop.last._2)
      originalLoop.dropRight(1) :+ loopStart
    }
    def item(pos: Pos): Char = problem.matrix(pos.y)(pos.x)
    def itemOnLoop(pos: Pos): Option[Char] = loop.filter(_._2 == pos).headOption.map(_._1)

    def loopItemsOnRow(y: Int) = LoopContents.loopItemsOnRow(loop, y)
    def loopItemsOnCol(x: Int) = LoopContents.loopItemsOnCol(loop, x)
    def isInLoop(pos: Pos) = {
      !itemOnLoop(pos).isDefined &&
        ( math.floorMod(loopItemsOnRow(pos.y).filter(_._2.x < pos.x).size, 2) == 1 ||
          math.floorMod(loopItemsOnCol(pos.x).filter(_._2.y < pos.y).size, 2) == 1 )
    }
    def printLoop: Unit =
      problem.matrix.zipWithIndex.map{ case (line, y) =>
        line.zipWithIndex.map { case (c, x) =>
          val pos = Pos(x = x, y = y)
          if(itemOnLoop(pos).isDefined) itemOnLoop(pos).get
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

  object LoopContents {
    val corners = Seq(NE, NW, SE, SW)
    def canInline(a: Char, b: Char): Boolean =
      (a == NW && b == SE) || (a == SW && b == NE) || (a == NE && b == SW) || (a == SE && b == NW)
    def isVertical(c: Char): Boolean = c == NS
    def isHorizontal(c: Char): Boolean = c == EW
    def isUTurn(a: Char, b: Char): Boolean =
      (a, b) match {
        case (NE, NW) => true
        case (SE, SW) => true
        case (SW, NW) => true
        case (SE, NE) => true
        case (_, _) => false
      }

    def findLoopStart(loop: Seq[(Char, Pos)]): Char = {
      val s = loop.last._2
      val p = loop.dropRight(1).last._2
      val n = loop.head._2
      val (v, h) = if (p.x == s.x) (p, n) else (n, p)
      if     (p.x == n.x)              NS
      else if(p.y == n.y)              EW
      else if(v.x < h.x && v.y > h.y)  SE
      else if(v.x > h.x && v.y > h.y)  SW
      else if(v.x > h.x && v.y < h.y)  NW
      else if(v.x < h.x && v.y < h.y)  NE
      else OO
    }

    def loopItemsOnRow(loop: Seq[(Char, Pos)], y: Int) = {
      loop
        .filter(_._2.y == y)
        .sortBy(_._2.x)
        .filterNot(x => isHorizontal(x._1))
        .foldLeft(Seq[(Char, Pos)]())((acc, p) =>
          acc match {
            case x +: xs if isUTurn(x._1, p._1) => xs
            case x +: xs if canInline(x._1, p._1) => (Vertical, p._2) +: xs
            case _ => p +: acc
            case Nil => acc
          }
        )
        .filter(x => isVertical(x._1))
        .reverse
    }

    def loopItemsOnCol(loop: Seq[(Char, Pos)], x: Int) = {
      loop.filter(_._2.x == x)
        .sortBy(_._2.y)
        .filterNot(x => isVertical(x._1))
        .foldLeft(Seq[(Char, Pos)]())((acc, p) =>
          acc match {
            case x +: xs if isUTurn(x._1, p._1) => xs
            case x +: xs if canInline(x._1, p._1) => (Horizontal, p._2) +: xs
            case _ => p +: acc
            case Nil => acc

          }
        ).filter(x => isHorizontal(x._1))
    }
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

  val problem22 = Problem(sample22)
  val resultSample22 = LoopContents(problem22).contents()
  println(s"Sample 22: ${resultSample22.size}")
  println(s"Sample 22: ${resultSample22}")

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

  val problem23 = Problem(sample23)
  val solver23 = LoopContents(problem23)
  val resultSample23 = solver23.contents()
  println(s"Sample 23: ${resultSample23.size}")

  val problem2 = Problem(input)
  val solver2 = LoopContents(problem2)
  val result2 = solver2.contents()
  println(s"Part 2: ${result2.size}") // 6725



}
