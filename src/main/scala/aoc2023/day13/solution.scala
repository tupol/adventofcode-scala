package aoc2023.day13

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  val sample: Seq[String] =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#
      |
      |""".stripMargin.split("\n").map(_.trim)

  def input = Source.fromResource("aoc2023/day13/input.txt").getLines().map(_.trim)

  // Part 1  ============================================================================================
  
  def pattern2Number(input: String): Int = {
    input.zipWithIndex.map{ case (c, p) =>
      val n = if(c == '#') 2 else 0
      if(p == 0 && n == 0) 0
      else math.pow(n, p).toInt
    }.sum
  }

  def rotate(pattern: Seq[String]): Seq[String] =
    pattern.foldLeft(Seq[Seq[Char]]()) { case (acc, row) =>
      acc match {
        case Nil => row.map(Seq(_))
        case acc => acc.zip(row).map((ax, c) => ax :+ c)
      }
    }.map(_.mkString(""))

  def mirrorLine(data: IndexedSeq[Int]) = {
    if(data.size < 2)
      None
    else
      val mid = data.size / 2
      if (math.floorMod(data.size, 2) == 0) {
        val (l, r) = data.splitAt(mid)
        if (l.zip(r.reverse).forall(_ == _)) Some(mid) else None
      } else if (data(0) == data(data.size - 2)) {
        val (l, r) = data.dropRight(1).splitAt(mid)
        if (l.zip(r.reverse).forall(_ == _)) Some(mid) else None
      } else if (data(1) == data(data.size - 1)) {
        val (l, r) = data.drop(1).splitAt(mid)
        if (l.zip(r.reverse).forall(_ == _)) Some(mid + 1) else None
      } else
        None
  }

  def mirror(pattern: Seq[String]): (Option[Int], Option[Int]) = {
    val row = mirrorLine(pattern.map(pattern2Number).toIndexedSeq)
    val col = mirrorLine(rotate(pattern).map(pattern2Number).toIndexedSeq)
    renderResult(pattern, row, col)
    (row, col)
  }

  def renderResult(pattern: Seq[String], row: Option[Int], col: Option[Int]) = {
    val maxCols = pattern.map(_.size).max
    val colInfo = col.map(c => (1 until c).map(_ => ' ') ++ "><" ++ (c+1 until maxCols).map(_ => ' ')).getOrElse(Seq[Char]()).mkString("")
    println
    println(s"  $colInfo")
    pattern.zipWithIndex.map{ case (line, i) =>
      row.map{c => if(i == c-1) s"v $line" else if (i == c) s"^ $line" else s"  $line"}.getOrElse(s"  $line")
    }.foreach(println)
  }

  def mirrors(input: Iterator[String]) = (input ++ Seq("\n") ).map(_.trim)
    .foldLeft((Seq[String](), Seq[(Option[Int], Option[Int])]())) { case ((accPat, accRes), line) =>
      (accPat, line) match {
        case (Nil, l) if l.isEmpty  => (accPat, accRes)
        case (data, l) if l.isEmpty => (Seq[String](), accRes :+ mirror(data))
        case (_, line)              => (accPat :+ line, accRes)
      }
    }._2

  val sampleResults = mirrors(sample.to(Iterator))
  println(sampleResults)
  val sampleResult = sampleResults.map((a, b) => (a.getOrElse(0), b.getOrElse(0))).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
  println(s"Sample 1: ${sampleResult._1*100 + sampleResult._2}")

  val results1 = mirrors(input)
  println(results1.map((a, b) => (a.getOrElse(0), b.getOrElse(0))))//.filter(x => x._1 != 0 || x._2 != 0))
  val result1 = results1.map((a, b) => (a.getOrElse(0), b.getOrElse(0))).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
  println(s"Part 1: ${result1._1*100 + result1._2}") // !(716 too low)

  // Part 2 ============================================================================================
  
  def isMirrored(data: Seq[Int]): Option[Int] = {
    if(math.floorMod(data.size, 2) == 0) {
      val (l, r) = data.splitAt(data.size / 2)
      if (l.zip(r.reverse).forall(_ == _)) Some(data.size / 2) else None
    } else
      None
  }

    def findMiddle(data: Seq[Int]): Option[(Int, Int)] = {
      def findInWindow(data: Seq[Int], windowSize: Int): Option[(Int, Int)] = {
        data.sliding(windowSize, 1).zipWithIndex
          .map { case (data, from) => isMirrored(data).map(x => (x + from, windowSize)) }
          .filterNot(_.isEmpty)
          .toSeq.headOption.flatten
      }
      if(data.size < 2) None
      else if (isMirrored(data).isDefined) isMirrored(data).map(x => (x, data.size))
      else
        (1 to data.size/2).map(_ * 2).reverse
          .foldLeft(Option.empty[(Int, Int)]){ (acc, ws) => if(acc.isDefined) acc else findInWindow(data, ws) }
    }

  def mirror2(pattern: Seq[String]): (Option[Int], Option[Int]) = {
    val maxRows = pattern.size
    val maxCols = pattern.map(_.size).max
    val rc = findMiddle(pattern.map(pattern2Number).toIndexedSeq)
    val cc = findMiddle(rotate(pattern).map(pattern2Number).toIndexedSeq)

    val (row, col) = (rc, cc) match {
      case (Some(r), Some(c)) =>
        val rr = maxRows - r._2
        val cr = maxCols - c._2
        if(rr >= cr) (None, Some(c._1)) else (Some(r._1), None)
      case (Some(r), None) => (Some(r._1), None)
      case (None, Some(c)) => (None, Some(c._1))
      case (None, None) => (None, None)
    }

    renderResult(pattern, row, col)
    (row, col)
  }

  def mirrors2(input: Iterator[String]) = (input ++ Seq("\n")).map(_.trim)
    .foldLeft((Seq[String](), Seq[(Option[Int], Option[Int])]())) { case ((accPat, accRes), line) =>
      (accPat, line) match {
        case (Nil, l) if l.isEmpty => (accPat, accRes)
        case (data, l) if l.isEmpty => (Seq[String](), accRes :+ mirror2(data))
        case (_, line) => (accPat :+ line, accRes)
      }
    }._2

//  val results12 = mirrors2(input.to(Iterator))
//  println(results12.map((a, b) => (a.getOrElse(0), b.getOrElse(0))).filter(x => x._1 != 0 || x._2 != 0))
//  val result12 = results12.map((a, b) => (a.getOrElse(0), b.getOrElse(0))).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
//  println(s"Part 1#2: ${result12._1 * 100 + result12._2}") // !(716 too low) // !32638


}
