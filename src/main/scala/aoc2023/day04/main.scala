package aoc2023.day04

import aoc2023.day02.main.result1
import aoc2023.day04.main.Card.*

import scala.io.Source
import scala.util.*

object main extends App {

  // Map[CardId, (CardsNumber, CardScore)]
  case class Cards(cards: Map[Int, (Int, Option[Int])] = Map()) {
    def addCardId(id: Int, number: Int = 1): Cards = {
      val ci = cards.get(id)
        .map{ case (count, score) => (count+number, score)}
        .getOrElse((number, None))
      Cards(cards + (id -> ci))
    }

    def copiesOf(id: Int): Int = cards.get(id).map { case (count, _) => count }.getOrElse(0)

    def addCardScore(id: Int, score: Int): Cards = {
      cards.get(id).flatMap {
        case (_, Some(_)) => None
        case (count, None) => Some(Cards(cards + (id -> (count, Some(score)))))
      }.getOrElse(this)
    }

    def print() =
      cards.map { case (id, (count, score)) => (id, count, score) }.toSeq
        .sortBy(_._1)
        .foreach(println)
  }
  case class Card(id: Int, win: Seq[Int], own: Seq[Int])
  object Card {
    val pat = """card *(\d+) *\:(.*)\|(.*)""".r
    def fromString(input: String) =
      input.toLowerCase match {
        case pat(id, win, own) =>
          val winn = win.trim.split(" +").map(_.toInt)
          val ownn = own.trim.split(" +").map(_.toInt)
          Success(Card(id.toInt, winn, ownn))
        case _ => Failure(new IllegalArgumentException(s"This line is not a valid card: $input"))
      }

    def matches(card: Card): Int = card.own.intersect(card.win).size
    def score(card: Card): Int = {
      val mx = matches(card)
      if(mx == 0) 0 else math.pow(2, mx-1).toInt
    }
  }

  val card = Card.fromString("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53").get

  val sample =
    """
      |Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

  def input = Source.fromResource("aoc2023/day04/input.txt").getLines()
  val result1 = input.map(c => score(Card.fromString(c).get)).sum
  println(s"Part 1: $result1")



  val result2 = input.map(c => Card.fromString(c).get)
    .foldLeft(Cards()) { case (cards, card) =>
      val score = matches(card)
      val cid = card.id
      val scoredCards = cards.addCardId(cid).addCardScore(cid, score)
      val copies = scoredCards.copiesOf(cid)
      val result = score match {
        case 0 => scoredCards
        case score =>
          (1 to score).foldLeft(scoredCards)((cards, i) => cards.addCardId(cid + i, copies))
      }
      result
    }.cards.map(_._2._1).sum
  println(s"Part 2: $result2")

}
