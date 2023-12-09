package aoc2023.day08

import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*
import aoc2023.day07.solution.Part1Rules

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {


  type Model = Map[String, (String, String)]
  val EmptyModel = Map[String, (String, String)]()

  object Model {
    val pattern = """([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)""".r
    def parseInput(input: String): Map[String, (String, String)] =
      input match {
        case pattern(name, left, right) => Map(name -> (left, right))
        case _ => Map()
      }
  }

  def samplePath = "RL"
  def sample1a =
    """
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

  def sample1b =
    """
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)


  val sampleModel11 = sample1a.foldLeft(EmptyModel)((acc, line) => acc ++ Model.parseInput(line))
  val sampleModel12 = sample1b.foldLeft(EmptyModel)((acc, line) => acc ++ Model.parseInput(line))

  def navigate(model: Model, instructions: Seq[Char]): Seq[String] = {
    def loop(path: Seq[String], workingInstructions: Seq[Char]): Seq[String] = {
      path match {
        case head +: _ if head == "ZZZ" => path
        case _ =>
          workingInstructions match {
            case 'R' +: ix => loop(model(path.head)._2 +: path, ix)
            case 'L' +: ix => loop(model(path.head)._1 +: path, ix)
            case Nil       => loop(path, instructions)
          }
      }
    }
    loop(Seq("AAA"), instructions)
  }

  def ghostNavigate(model: Model, instructions: Seq[Char]) = {

    def loop(current: String, steps: Long, workingInstructions: Seq[Char]): (String, Long) = {
      //      println(f"${current.mkString("(", "-", ")")} $steps%6d")
      current match {
        case x if x.endsWith("Z") => (x, steps)
        case _ =>
          workingInstructions match {
            case 'R' +: ix => loop(model(current)._2, steps+ 1, ix)
            case 'L' +: ix => loop(model(current)._1, steps+ 1, ix)
            case Nil => loop(current, steps, instructions)
          }
      }
    }

    def gcd(a: Long, b: Long): Long =
      b match {
        case b if b == 0 => a
        case b =>
          val min = math.min(a, b)
          val max = math.max(a, b)
          val nb = math.floorMod(max, min)
          val na = min
          gcd(na, nb)
      }

    def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

    val start = model.keys.filter(_.endsWith("A")).toSeq.sorted

    val steps = start.map(s => (s, loop(s, 0, instructions))).map(x => (x._1, x._2._1, x._2._2))

    steps.map(_._3).reduce(lcm)
  }

  val sampleSolution1 = navigate(sampleModel11, "RL")
  println(s"Sample 1a: ${sampleSolution1.size - 1}")

  val sampleSolution2 = navigate(sampleModel12, "LLR")
  println(s"Sample 1b: ${sampleSolution2.size - 1}")

  def input = Source.fromResource("aoc2023/day08/input.txt").getLines()
  val model =  input.foldLeft(EmptyModel)((acc, line) => acc ++ Model.parseInput(line))
  val instructions = "LLRRRLLRLRRRLLRLRLRLRLRRRLRRLRRLRLLLRRLLRRLRRLRRLRRRLLLRRLRLRRRLRRRLRLRRLRRRLRLRRRLRLRLLLRLRRLRLRRLRRRLRLRRRLRRRLRRRLRRRLRLRRRLRRRLRLLRRLRLRLRRRLRRLRRRLRRRLRRRLRRRLLLLRRLLRLRRLRRLRRRLRRRLLLRRLRRLRLRRLRRRLRRLRLRRRLRLRRLLRLLRRLRLRRRLRRLRRLRLRRLLLRRRLRLRRRLRLRLLRLRLRRRLRLRLRRRLRRLRRLRRRLRRLLRRRR"
  val solution1 = navigate(model, instructions)
  println(s"Part 1: ${solution1.size - 1}")

  def sample2 =
    """
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

  val sampleModel2 = sample2.foldLeft(EmptyModel)((acc, line) => acc ++ Model.parseInput(line))
  println(s"Sample 2: ${ghostNavigate(sampleModel2, "LR")}")

  val solution2 = ghostNavigate(model, instructions)
  println(s"Part 2: ${solution2}")

}
