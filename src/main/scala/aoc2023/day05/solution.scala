package aoc2023.day05

import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object solution extends App {

  sealed trait Kind {
    val name: String
    override def toString: String = name
  }
  object Kind {
    case object Seed extends Kind {
      val name = "seed"
    }
    case object Soil extends Kind {
      val name = "soil"
    }
    case object Fertilizer extends Kind {
      val name = "fertilizer"
    }
    case object Water extends Kind {
      val name = "water"
    }
    case object Light extends Kind {
      val name = "light"
    }
    case object Temperature extends Kind {
      val name = "temperature"
    }
    case object Humidity extends Kind {
      val name = "humidity"
    }
    case object Location extends Kind {
      val name = "location"
    }
    def fromString(input: String): Try[Kind] = input.trim.toLowerCase() match {
      case Seed.name => Success(Seed)
      case Soil.name => Success(Soil)
      case Fertilizer.name => Success(Fertilizer)
      case Water.name => Success(Water)
      case Light.name => Success(Light)
      case Temperature.name => Success(Temperature)
      case Humidity.name => Success(Humidity)
      case Location.name => Success(Location)
      case _ => Failure(new IllegalArgumentException(s"$input is an unknown kind."))
    }
  }

  import Kind._

  case class Thing(id: Long, kind: Kind)

  case class Range(destStart: Long, srcStart: Long, length: Long){
    def contains(source: Long): Boolean = {
      source >= srcStart && source < (srcStart + length)
    }
    def destination(source: Long): Option[Long] =
      if(contains(source)) Some(destStart + source - srcStart)
      else None
    override def toString: String = s"$destStart, $srcStart, $length"
  }

  object Range {
    val pat = """(\d+) +(\d+) +(\d+)""".r
    def fromString(input: String): Try[Range] =
      input.trim.toLowerCase match {
        case pat(destStart, srcStart, length) => Success(Range(destStart.toLong, srcStart.toLong, length.toLong))
        case _ => Failure(new IllegalArgumentException(s"This line is not a valid range: $input"))
      }
  }

  case class Mapper(srcKind: Kind, destKind: Kind, ranges: Set[Range] = Set()) {
    val name = s"$srcKind-to-$destKind"
    require(srcKind != destKind, s"A range can not be initialized with the same source and destination kinds: $srcKind / $destKind")
    def withRange(range: Range): Mapper = this.copy(ranges = ranges ++ Set(range))
    def destination(source: Thing): Thing = {
      require(source.kind == srcKind, s"This range only accepts $srcKind sources, but it was given ${source.kind}")
      val results = ranges.map(_.destination(source.id)).filter(_.isDefined)
      if(results.size > 1) throw new Exception(s"$name has more ranges containing the same source: $source.")
      val id = results.headOption.flatten.getOrElse(source.id)
      Thing(id, destKind)
    }

    def destinationId(source: Long): Long = {
      val results = ranges.map(_.destination(source)).filter(_.isDefined)
      if (results.size > 1) throw new Exception(s"$name has more ranges containing the same source: $source.")
      results.headOption.flatten.getOrElse(source)
    }

    override def toString: String =
      s"""$name map:
        |${ranges.mkString("\n")}
        |""".stripMargin
  }

  object Mapper {

    val pat = """(\w*)-to-(\w*) +map:""".r

    def fromString(input: String): Try[Mapper] =
      input.trim.toLowerCase match {
        case pat(srcKind, destKind) =>
          for {
            sk <- Kind.fromString(srcKind)
            dk <- Kind.fromString(destKind)
          } yield Mapper(sk, dk)
        case _ => Failure(new IllegalArgumentException(s"This line is not a Mapper: $input"))
      }
   }

  object Seeds {
    val pat = """seeds:(.+)""".r
    def fromString(input: String): Option[Seq[Long]] =
      input.trim.toLowerCase match {
        case pat(ids) => Some(ids.split(" ").map(_.trim).filterNot(_.isEmpty).map(_.toLong))
        case _ => None
      }
  }

  case class ProblemDefinition(seeds: Iterator[Long] = Iterator(), mappers: Seq[Mapper] = Seq()) {

    val mappersMap = mappers.map(m => (m.srcKind, m)).toMap

    def addMapper(mapper: Mapper): ProblemDefinition = this.copy(mappers = mapper +: mappers)
    def addRange(range: Range): ProblemDefinition = mappers match {
      case Nil => throw new IllegalStateException("We try to add a range, but we have no Mapper yet")
      case mapper +: tail => this.copy(mappers = mapper.withRange(range) +: tail)
    }

    override def toString: String =
      s"""seeds: ${seeds.take(100).toSeq.sorted.mkString(" ")}...
         |${mappers.reverse.mkString("\n")}
         |""".stripMargin
  }

  object ProblemDefinition {
    def apply(input: Iterator[String]): ProblemDefinition =
      input.foldLeft(ProblemDefinition()) { (problem, line) =>
        val seeds = Seeds.fromString(line)
        val mapper = Mapper.fromString(line).toOption
        val range = Range.fromString(line).toOption
        (seeds, mapper, range) match {
          case (Some(s), _, _) => problem.copy(seeds = s.iterator)
          case (_, Some(m), _) => problem.addMapper(m)
          case (_, _, Some(r)) => problem.addRange(r)
          case _ => problem
        }
      }
  }

  object ProblemSolver {

    private def createMapperFunction1(problem: ProblemDefinition): Thing => Thing = {
      @tailrec
      def loop(srcKind: Kind, mappers: Seq[Mapper]): Thing => Thing =
        problem.mappersMap.get(srcKind) match {
          case None => mappers.reverse.map(_.destination).reduceOption(_ andThen _).getOrElse(identity)
          case Some(m) => loop(m.destKind, m +: mappers)
        }
      loop(Seed, Seq())
    }

    def resolvePart1(problem: ProblemDefinition): Long = {
      val map1: Thing => Thing = createMapperFunction1(problem)
      problem.seeds.map { s =>
        val seed = Thing(s, Seed)
        val location: Thing = map1(seed)
        (seed, location)
      }.map(_._2.id).min
    }

    private def createMapperFunction2(problem: ProblemDefinition): Long => Long = {
      def loop(srcKind: Kind, mappers: Seq[Mapper]): Long => Long =
        problem.mappersMap.get(srcKind) match {
          case None => mappers.reverse.map(_.destinationId).reduceOption(_ andThen _).getOrElse(identity)
          case Some(m) => loop(m.destKind, m +: mappers)
        }

      loop(Seed, Seq())
    }

    def resolvePart2(problem: ProblemDefinition): Long = {
      import scala.concurrent._
      implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(20))

      def allOkOrFail[T](futures: Iterable[Future[T]])(implicit ec: ExecutionContext): Future[Iterable[T]] = {
        import scala.collection.immutable.Vector
        futures.foldLeft(Future.successful(Vector.empty[T])) { (acc, future) =>
          future.flatMap(tf => acc.map(_ :+ tf))
        }
      }

      val chunkSize = 10000000
      val location: Long => Long = createMapperFunction2(problem)
      problem.seeds.sliding(chunkSize, chunkSize).zipWithIndex.map { case (sx, batch) =>
        val window = chunkSize / 20
        val fx = sx.sliding(window, window).map { sx =>
          Future {
            val res = sx.map { s => location(s) }.min
            res
          }
        }.to(Iterable)
        Await.result(allOkOrFail(fx), Duration.Inf).min
      }.min
    }

  }

  val sample = Source.fromResource("aoc2023/day05/sample.txt").getLines()
  val sampleProblem = ProblemDefinition(sample)
  println(s"Sample 1: ${ProblemSolver.resolvePart1(sampleProblem)}")

  def input = Source.fromResource("aoc2023/day05/input.txt").getLines()
  val problem1 = ProblemDefinition(input)
  println(s"Part 1: ${ProblemSolver.resolvePart1(problem1)}")

  def fixSeeds(problem: ProblemDefinition): ProblemDefinition = {
    val fixedSeeds = problem.seeds.sliding(2, 2).map {
      case (a +: b +: Nil) => (0 until b.toInt).iterator.map(_ + a)
      case _ => Iterator()
    }.reduce(_ ++ _)
    problem.copy(seeds = fixedSeeds)
  }

  val problem2 = fixSeeds(ProblemDefinition(input))
  // Patience...
  println(s"Part 2: ${ProblemSolver.resolvePart2(problem2)}")

}
