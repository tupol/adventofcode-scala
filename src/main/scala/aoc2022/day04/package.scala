package aoc2022

import scala.io.Source
import scala.util.{Failure, Try}

package object day04 {

  def input(path: String): Iterator[(Interval, Interval)] = Source.fromResource(path).getLines()
    .map { line =>
      val res = line.split(",").map(Interval.fromString)
      (res.head.get, res.tail.head.get)
    }
  
  case class Interval(from: Int, to: Int) {
    def contains(value: Int): Boolean = value >= from && value <= to
    def isFullyContainedIn(that: Interval): Boolean = that.contains(from) && that.contains(to)

    def isPartiallyContainedIn(that: Interval): Boolean = that.contains(from) || that.contains(to)
  }

  object Interval {
    def fromString(input: String): Try[Interval] = {
      input.split("-").toList match
        case l :: r :: Nil => Try(Interval(l.toInt, r.toInt))
        case _ => Failure(new IllegalArgumentException("Parse of interval failed"))
    }
  }

}
