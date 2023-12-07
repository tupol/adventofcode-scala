package aoc2023.day07


import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*
import aoc2023.day07.solution.Part1Rules

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  sealed trait Card
  object Card {
    case object C2 extends Card {
      override def toString: String = "2";
    }
    case object C3 extends Card {
      override def toString: String = "3";
    }
    case object C4 extends Card {
      override def toString: String = "4";
    }
    case object C5 extends Card {
      override def toString: String = "5";
    }
    case object C6 extends Card {
      override def toString: String = "6";
    }
    case object C7 extends Card {
      override def toString: String = "7";
    }
    case object C8 extends Card {
      override def toString: String = "8";
    }
    case object C9 extends Card {
      override def toString: String = "9";
    }
    case object CT extends Card {
      override def toString: String = "T";
    }
    case object CJ extends Card {
      override def toString: String = "J";
    }
    case object CQ extends Card {
      override def toString: String = "Q";
    }
    case object CK extends Card {
      override def toString: String = "K";
    }
    case object CA extends Card {
      override def toString: String = "A";
    }

    private val inputMap = Map(
      "2" -> C2,
      "3" -> C3,
      "4" -> C4,
      "5" -> C5,
      "6" -> C6,
      "7" -> C7,
      "8" -> C8,
      "9" -> C9,
      "T" -> CT,
      "J" -> CJ,
      "Q" -> CQ,
      "K" -> CK,
      "A" -> CA
    )
    def fromString(input: String): Option[Card] = inputMap.get(input.trim)
  }


  sealed trait HandType {
    def bumpHand: HandType
  }
  object HandType {
    case object HighCard extends HandType {
      def bumpHand = OnePair
    }
    case object OnePair extends HandType {
      def bumpHand = ThreeOfAKind
    }
    case object TwoPair extends HandType {
      def bumpHand = FullHouse
    }
    case object ThreeOfAKind extends HandType {
      def bumpHand = FourOfAKind
    }
    case object FullHouse extends HandType {
      def bumpHand = FourOfAKind
    }
    case object FourOfAKind extends HandType {
      def bumpHand = FiveOfAKind
    }
    case object FiveOfAKind extends HandType {
      def bumpHand = FiveOfAKind
    }
  }


  case class Hand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) {
    val cards = Seq(c1, c2, c3, c4, c5)
    override def toString: String = cards.mkString("")
  }
  object Hand{
    def fromString(input: String): Hand = {
      val parsedCards = input.trim.map(c => Card.fromString(c.toString)).collect{ case Some(c) => c }
      parsedCards match {
        case Seq(c1, c2, c3, c4, c5) => Hand(c1, c2, c3, c4, c5)
        case cx => throw new IllegalAccessException(s"A hand can have 5 and only 5 cards, but we got ${cx.size} in ${cx.mkString("")}")
      }
    }
  }

  case class PlayHand(hand: Hand, bet: Long)
  object PlayHand {
    def fromString(input: String): PlayHand =
      input.trim.split(" ").toSeq match
        case Seq(hand, bet) => PlayHand(Hand.fromString(hand), bet.toLong)
        case _ => throw new IllegalAccessException(s"The input does not look as a playing hand: $input")
  }

  def handOrdering(cardValue: Card => Int, handTypeValue: Hand => Int) = new Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      if (handTypeValue(x) < handTypeValue(y))
        -1
      else if (handTypeValue(x) > handTypeValue(y))
        1
      else {
        x.cards.zip(y.cards).foldLeft(0) { case (ord, (xc, yc)) =>
          if (ord == 0) Ordering[Int].compare(cardValue(xc), cardValue(yc))
          else ord
        }
      }
    }
  }

  object Part1Rules {
    private def cardsOrderMap: Map[Card, Int] = Seq(C2, C3, C4, C5, C6, C7, C8, C9, CT, CJ, CQ, CK, CA).zipWithIndex.toMap
    private def handsOrderMap: Map[HandType, Int] = Seq(HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind).zipWithIndex.toMap

    def cardValue(card: Card) = cardsOrderMap(card)
    def handTypeValue(hand: Hand): Int = handsOrderMap(handType(hand))

    implicit val HandOrdering: Ordering[Hand] = handOrdering(cardValue, handTypeValue)

    def prettyHand(hand: Hand): String = hand.cards.sortBy(cardValue).reverse.mkString("")

    def handType(hand: Hand): HandType = {
      val cardGroups = hand.cards.groupBy(identity).map { case (c, cx) => (c, cx.size) }.toSeq.sortBy(_._2).reverse
      cardGroups match {
        case (card, 5) +: Nil => FiveOfAKind
        case (card1, 4) +: (card2, count2) +: Nil => FourOfAKind
        case (card1, 3) +: (card2, 2) +: Nil => FullHouse
        case (card1, 3) +: _ => ThreeOfAKind
        case (card1, 2) +: (card2, 2) +: _ => TwoPair
        case (card1, 2) +: _ => OnePair
        case _ => HighCard
      }
    }
    def winnings(playHands: Seq[PlayHand]): Long = {
      val rankedHands = playHands.sortBy(_.hand).zipWithIndex.map { case (ph, ix) => (ph, (ix + 1)) }
//      rankedHands.map(rh => f"${rh._1.hand}  ${rh._1.bet}%4d$$  ${rh._2}%4d  |  ${prettyHand(rh._1.hand)} ${handType(rh._1.hand)}").foreach(println)
//          rankedHands.map(rh => f"${rh._1.bet},${rh._2}").foreach(println)
      val amount = rankedHands.map { case (ph, rank) => ph.bet * rank }.sum
      amount
    }
  }

  object Part2Rules {
    private def cardsOrderMap: Map[Card, Int] = Seq(CJ, C2, C3, C4, C5, C6, C7, C8, C9, CT, CQ, CK, CA).zipWithIndex.toMap

    private def handsOrderMap: Map[HandType, Int] = Seq(HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind).zipWithIndex.toMap

    def cardValue(card: Card) = cardsOrderMap(card)

    def handTypeValue(hand: Hand): Int = handsOrderMap(handType(hand))

    implicit val HandOrdering: Ordering[Hand] = handOrdering(cardValue, handTypeValue)

    def prettyHand(hand: Hand): String = hand.cards.sortBy(cardValue).reverse.mkString("")

    def handType(hand: Hand): HandType = {
      val initialRank = Part1Rules.handType(hand)
      if (initialRank == OnePair && hand.cards.filter(_ == CJ).size == 2)
        ThreeOfAKind
      else
        hand.cards.filter(_ == CJ).foldLeft(initialRank)((r, _) => r.bumpHand)
    }

    def winnings(playHands: Seq[PlayHand]): Long = {
      val rankedHands = playHands.sortBy(_.hand).zipWithIndex.map { case (ph, ix) => (ph, (ix + 1)) }
//      rankedHands.map(rh => f"${rh._1.hand}  ${rh._1.bet}%4d$$  ${rh._2}%4d  |  ${prettyHand(rh._1.hand)} ${handType(rh._1.hand)}").foreach(println)
//      rankedHands.map(rh => f"${rh._1.bet},${rh._2}").foreach(println)
      val amount = rankedHands.map { case (ph, rank) => ph.bet * rank }.sum
      amount
    }
  }


  val sampleCards =
    """
      |32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)


  val sampleResult1 = Part1Rules.winnings(sampleCards.map(PlayHand.fromString))
  println(s"Sample 1: $sampleResult1") // 288

  def input = Source.fromResource("aoc2023/day07/input.txt").getLines()
  val result1 = Part1Rules.winnings(input.map(PlayHand.fromString).toSeq)
  println(s"Part 1: $result1") // 252656917

  val sampleResult2 = Part2Rules.winnings(sampleCards.map(PlayHand.fromString))
  println(s"Sample 2: $sampleResult2") // 5905

  val result2 = Part2Rules.winnings(input.map(PlayHand.fromString).toSeq)
  println(s"Part 2: $result2") // 253499763
}
