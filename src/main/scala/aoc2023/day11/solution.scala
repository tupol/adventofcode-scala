package aoc2023.day11

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  case class Pos(y: Long, x: Long) {
    override def toString: String = s"($y, $x)"
  }
  case class Matrix(matrix: IndexedSeq[IndexedSeq[Char]]) {
    def item(pos: Pos): Char = matrix(pos.y.toInt)(pos.x.toInt)

    def emptyRows(): Seq[Long] = {
      matrix.zipWithIndex. map{ case (line, y) =>
        (y, line.forall(_ == '.'))
      }.filter(_._2).map(_._1)
    }

    def emptyCols(): Seq[Long] = {
      (0 until matrix(0).size).map { x =>
        (x, matrix.zipWithIndex.map{ case (line, y) => line(x)}.forall(_ == '.') )
      }.filter(_._2).map(_._1)
    }
    def expandEmptyRows(): Matrix = {
      val empty = emptyRows()
      Matrix(
        matrix.zipWithIndex.foldLeft(Empty){ (acc, y) =>
          if(empty.contains(y._2)) acc :+ y._1 :+ y._1
          else acc :+ y._1
        }
      )
    }

    def expandEmptyCols(): Matrix = {
      val empty = emptyCols()
      Matrix(
        matrix.map{ line =>
          val expLine = line.zipWithIndex.foldLeft(Seq.empty[Char]){ (acc, x) =>
            if (empty.contains(x._2)) acc :+ x._1 :+ x._1
            else acc :+ x._1
          }
          expLine.toIndexedSeq
        }
      )
    }
    def expandEmptySpaces(): Matrix = this.expandEmptyCols().expandEmptyRows()

    def galaxies(): Seq[(Int, Pos)] = matrix.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.filter(_._1 == '#').map(x => Pos(y, x._2))
    }.zipWithIndex.map{ case (pos, idx) => (idx+1, pos)}


    override def toString(): String =
      matrix.map { _.mkString("")}.mkString("\n")
  }

  val Empty: IndexedSeq[IndexedSeq[Char]] = IndexedSeq.empty[IndexedSeq[Char]]

  def distance(p1: Pos, p2: Pos): Long = (math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)).toLong

  def distances(galaxies: Seq[(Int, Pos)]) = for {
    g1 <- galaxies
    g2 <- galaxies
    if (g1 != g2)
    d = math.abs(g1._2.x - g2._2.x) + math.abs(g1._2.y - g2._2.y)
  } yield (g1, g2, d)

  val sample =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....
      |""".stripMargin.split("\n").map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq

  def input = Source.fromResource("aoc2023/day11/input.txt").getLines().map(_.trim.toIndexedSeq).filterNot(_.isEmpty).toIndexedSeq


  val sampleMap = Matrix(sample)
  val expandedMap = sampleMap.expandEmptySpaces()
  val sampleGalaxies = expandedMap.galaxies()
  val sampleResult1 = distances(sampleGalaxies).map(_._3).sum/ 2
  println(s"Sample 1: $sampleResult1") // 374

  val inputMap = Matrix(input).expandEmptySpaces()
  val inputGalaxies = inputMap.galaxies()
  val result1 = distances(inputGalaxies).map(_._3).sum / 2
  println(s"Part 1: $result1") // 10289334


  case class Matrix2(source: Matrix, expansionFactor: Long = 2) {
    val emptyRows = source.emptyRows()
    val emptyCols = source.emptyCols()
    val galaxies = source.galaxies()
    def expandRow(y: Long): Long = emptyRows.filter(_ < y).map(_ => expansionFactor - 1).sum + y

    def expandCols(x: Long): Long = emptyCols.filter(_ < x).map(_ => expansionFactor - 1).sum + x

    def expandedGalaxies: Seq[(Int, Pos)] = galaxies
      .map{ case (i, pos) => (i, pos.copy(y = expandRow(pos.y), x = expandCols(pos.x))) }
  }

  val m2Sample = Matrix2(sampleMap, 10)
  val sampleResult2 = distances(m2Sample.expandedGalaxies).map(_._3).sum / 2
  println(s"Sample 2: $sampleResult2") // 1030

  val m3Sample = Matrix2(sampleMap, 100)
  val sampleResult3 = distances(m3Sample.expandedGalaxies).map(_._3).sum / 2
  println(s"Sample 3: $sampleResult3") // 8410

  val m2 = Matrix2(Matrix(input), 1000000)
  val result2 = distances(m2.expandedGalaxies).map(_._3).sum / 2
  println(s"Part 2: $result2") // 649862989626
}
