package aoc2023.day19

import aoc2023.day16.solution.sampleResult1

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
    .toSeq

  val sampleInput =
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

  type Parts = Map[String, Int]
  object Parts {
    val pattern = "\\{(.+)\\}".r
    def fromString(input: String): Try[Parts] = input match {
      case pattern(in) =>
        val parts: Parts = in
          .split(",")
          .map(_.trim)
          .map { x =>
            val r = x.split("=")
            (r.head -> r.tail.head.toInt)
          }
          .toMap
        Success(parts)
      case _ => Failure(new Exception("No luck with this one :("))
    }
  }

  sealed trait Rule {
    def eval(parts: Parts): Rule
  }

  case object Accept extends Rule {
    def eval(parts: Parts): Rule = Accept
  }
  case object Reject extends Rule {
    def eval(parts: Parts): Rule = Reject
  }
  case class NextWorkFlow(id: String) extends Rule {
    def eval(parts: Parts): Rule = this
  }
  case object NextRule extends Rule {
    def eval(parts: Parts): Rule = NextRule
  }
  case class Predicate(item: String, predicate: Int => Boolean, trueBranch: Rule) extends Rule {
    def eval(parts: Parts): Rule =
      parts
        .get(item)
        .map(part => predicate(part))
        .getOrElse(true) match {
        case true  => trueBranch
        case false => NextRule
      }

  }
  case class Workflow(id: String, rules: Seq[Rule], status: Option[Boolean] = None)

  object Rule {
    val `<` = "([a-z]+)<([0-9]+):([a-zA-Z]+)".r
    val `>` = "([a-z]+)>([0-9]+):([a-zA-Z]+)".r
    def fromString(input: String): Rule = input.trim match {
      case `<`(id, comp, next) => Predicate(item = id, (a: Int) => a < comp.toInt, fromString(next))
      case `>`(id, comp, next) => Predicate(item = id, (a: Int) => a > comp.toInt, fromString(next))
      case "A"                 => Accept
      case "R"                 => Reject
      case wid                 => NextWorkFlow(wid)
    }
  }
  object Workflow {
    val pattern = "([a-z]+)\\{(.+)\\}".r
    def fromString(input: String): Try[Workflow] = input match {
      case pattern(id, rules) => Success(Workflow(id, rules.split(",").map(_.trim).map(Rule.fromString)))
      case _                  => Failure(new Exception("No luck with this one :("))
    }

    def eval(workflow: Workflow, parts: Parts)(implicit
      findWorkflow: (String) => Workflow
    ): Seq[Workflow] = {
      def loop(workflow: Workflow, parts: Parts, rules: Seq[Rule], path: Seq[Workflow])(implicit
        findWorkflow: (String) => Workflow
      ): Seq[Workflow] =
        rules match {
          case Nil => path
          case rule +: rest =>
            rule match {
              case Accept       => workflow.copy(status = Some(true)) +: path
              case Reject       => workflow.copy(status = Some(false)) +: path
              case NextRule     => loop(workflow, parts, rest, path)
              case p: Predicate => loop(workflow, parts, p.eval(parts) +: rest, path)
              case NextWorkFlow(id) =>
                val wf = findWorkflow(id)
                loop(wf, parts, wf.rules, workflow +: path)
            }
        }
      loop(workflow, parts, workflow.rules, Seq()).reverse

    }
  }

  // Part 1 ============================================================================================

  def inputData(input: Seq[String]): (Map[String, Workflow], Seq[Parts]) = {
    val tmp       = input.map(line => (Parts.fromString(line), Workflow.fromString(line)))
    val parts     = tmp.map(_._1).collect { case Success(x) => x }
    val workflows = tmp.map(_._2).collect { case Success(x) => (x.id, x) }.toMap
    (workflows, parts)
  }

  val (sampleWorkflows, sampleParts) = inputData(sampleInput)

  val sampleInFlow = sampleWorkflows.get("in").get

  val sampleResult1 = sampleParts.map { part =>
    val res    = Workflow.eval(sampleInFlow, part)(sampleWorkflows.get(_).get)
    val status = res.last.status
    (part.map(_._2).sum, status.getOrElse(false), res.map(_.id))
  }.filter(_._2).map(_._1).sum
  println(s"Sample 1: $sampleResult1") // 19114

  val (workflows, parts) = inputData(input)

  val inFlow = workflows.get("in").get

  val result1 = parts.map { part =>
    val res    = Workflow.eval(inFlow, part)(workflows.get(_).get)
    val status = res.last.status
    (part.map(_._2).sum, status.getOrElse(false), res.map(_.id))
  }.filter(_._2).map(_._1).sum
  println(s"Result 1: $result1") // 489392


}
