package aoc2023.day15

import aoc2023.day13.solution.sampleResult

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  def input = Source.fromResource("aoc2023/day15/input.txt").getLines().map(_.trim).filterNot(_.isEmpty).reduce(_ ++ _)
  val sample = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

  // Part 1 ============================================================================================

  def hash(value: Int, prevHash: Int = 0) = math.floorMod((value + prevHash) * 17, 256)
  def hashString(input: String) = input.toCharArray.foldLeft(0){ case (p, c) => hash(c, p) }

  val sampleResult = sample.split(",").map(s => (s, hashString(s))).map(_._2).sum
  println(s"Sample 1: $sampleResult")

  val result1 = input.split(",").map(s => (s, hashString(s))).map(_._2).sum
  println(s"Part 1: $result1") // 497373

  // Part 2 ============================================================================================

  case class Lens(id: String, focalLength: Int) {
    val hash = hashString(id)
  }
  case class LensBox(lenses: ArrayBuffer[Lens] = ArrayBuffer.empty[Lens]) {
    def removeLens(id: String): LensBox =
      lenses.zipWithIndex
        .filter(_._1.id == id).map(_._2).headOption match {
        case Some(i) => lenses.remove(i); this
        case None => this
      }

    def addLens(lens: Lens): LensBox =
      lenses.zipWithIndex
        .filter(_._1.id == lens.id).map(_._2).headOption match {
        case Some(i) => lenses.update(i, lens); this
        case None => lenses.addOne(lens); this
      }

    def isEmpty: Boolean = lenses.isEmpty

    def focusLength: Int = lenses.zipWithIndex.map{ case(l, i) => l.focalLength * (i + 1)}.sum
  }
  case class Telescope(private val lensBoxes: ArrayBuffer[LensBox] = ArrayBuffer.from((0 to 256).map(_ => LensBox()))) {
    def process(str: String): Telescope = {
      val id = str.takeWhile(c => !"-=".contains(c))
      val pos = hashString(id)
      if(str.contains('-')) lensBoxes(pos).removeLens(id)
      else lensBoxes(pos).addLens(Lens(id, str.split("=").last.toInt))
      this
    }

    def focusLength: Int = lensBoxes.zipWithIndex.filterNot(_._1.isEmpty)
      .map { case (l, i) => l.focusLength * (i+1) }.sum

    override def toString: String = lensBoxes.zipWithIndex.filterNot(_._1.isEmpty)
      .map{ case (box, i) => f"$i%3d  $box" }
      .mkString("\n")
  }

  val sampleTelescope = Telescope()
  sample.split(",").map(sampleTelescope.process)
  println(s"Sample 2: ${sampleTelescope.focusLength}")

  val telescope = Telescope()
  input.split(",").map(telescope.process)
  println(s"Part 2: ${telescope.focusLength}") // 259356

}
