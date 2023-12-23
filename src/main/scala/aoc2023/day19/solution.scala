package aoc2023.day19

import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.*
import scala.util.*

object solution extends App {

  def input = Source
    .fromResource("aoc2023/day19/input.txt")
    .getLines()
    .map(_.trim)

  val sampleWorkflows =
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}
      |""".stripMargin
      .split("\n")
      .toIndexedSeq
      .map(_.trim)


  sealed trait Rule

  case object Accept extends Rule
  case object Reject extends Rule
  case class Predicate(item: String, predicate: (Int, Int) => Boolean, trueBranch: () => Rule) extends Rule {

  }
  case class Workflow(id: String, rules: Seq[Rule], flow: Seq[Workflow], status: Option[Boolean] = None)(implicit findWorkflow: (String) => Workflow) extends Rule {
    def eval(rules: Seq[Rule]): Workflow = rules match {
      case Nil => this
      case r +: rx => r match {
        case Accept => this.copy(status = Some(true))
        case Reject => this.copy(status = Some(false))
        case p: Predicate =>

      }
    }
  }

  object Rule {
    def fromString(input: String): Rule = input.trim match {
      case "A" => Accept
      case "R" => Reject
    }
  }



  // Part 1 ============================================================================================


}
