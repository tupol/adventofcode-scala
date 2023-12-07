package aoc2023.day07


import aoc2023.day07.solution.Card.*
import aoc2023.day07.solution.HandType.*

import scala.io.Source
import scala.math
import scala.util.*

object solution extends App {

  sealed trait Card {
    def value1: Int

    def value2: Int = value1
  }
  object Card {

    case object C2 extends Card {
      val value1 = 2;
      override def toString: String = "2";
    }

    case object C3 extends Card {
      val value1 = 3
      override def toString: String = "3";
    }

    case object C4 extends Card {
      val value1 = 4;
      override def toString: String = "4";
    }

    case object C5 extends Card {
      val value1 = 5;
      override def toString: String = "5";
    }

    case object C6 extends Card {
      val value1 = 6;
      override def toString: String = "6";
    }

    case object C7 extends Card {
      val value1 = 7;
      override def toString: String = "7";
    }

    case object C8 extends Card {
      val value1 = 8;
      override def toString: String = "8";
    }

    case object C9 extends Card {
      val value1 = 9;
      override def toString: String = "9";
    }

    case object CT extends Card {
      val value1 = 10;
      override def toString: String = "T";
    }

    case object CJ extends Card {
      val value1 = 11;

      override val value2 = 1;
      override def toString: String = "J";
    }

    case object CQ extends Card {
      val value1 = 12;
      override def toString: String = "Q";
    }

    case object CK extends Card {
      val value1 = 13;
      override def toString: String = "K";
    }

    case object CA extends Card {
      val value1 = 14;
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
    def value: Int
    def bumpHand: HandType
  }
  object HandType {
    case object HighCard extends HandType {
      val value = 0
      def bumpHand = OnePair
    }
    case object OnePair extends HandType {
      val value = 1
      def bumpHand = ThreeOfAKind
    }
    case object TwoPair extends HandType {
      val value = 2
      def bumpHand = FullHouse
    }
    case object ThreeOfAKind extends HandType {
      val value = 3
      def bumpHand = FourOfAKind
    }
    case object FullHouse extends HandType {
      val value = 4
      def bumpHand = FourOfAKind
    }
    case object FourOfAKind extends HandType {
      val value = 5
      def bumpHand = FiveOfAKind
    }
    case object FiveOfAKind extends HandType {
      val value = 6
      def bumpHand = FiveOfAKind
    }
  }


  case class Hand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) {
    val cards = Seq(c1, c2, c3, c4, c5)
    lazy val handType1: HandType = {
      val cardGroups = cards.groupBy(identity).map { case (c, cx) => (c, cx.size) }.toSeq.sortBy(_._2).reverse
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
    lazy val handType2 = {
      val initialRank = handType1
      if(handType1 == OnePair && cards.filter(_ == CJ).size == 2)
        ThreeOfAKind
      else
        cards.filter(_ == CJ).foldLeft(initialRank)((r, _) => r.bumpHand)
    }
    override def toString: String = cards.mkString("")
    def toGrouppedString: String = cards.sortBy(_.value1).reverse.mkString("")
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

  val Hand1Ordering: Ordering[Hand] = new Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      if (x.handType1.value < y.handType1.value)
        -1
      else if (x.handType1.value > y.handType1.value)
        1
      else {
        x.cards.zip(y.cards).foldLeft(0) { case (ord, (xc, yc)) =>
          if (ord == 0) Ordering[Int].compare(xc.value1, yc.value1)
          else ord
        }
      }
    }
  }
  val Hand2Ordering: Ordering[Hand] = new Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      if (x.handType2.value < y.handType2.value)
        -1
      else if (x.handType2.value > y.handType2.value)
        1
      else {
        x.cards.zip(y.cards).foldLeft(0) { case (ord, (xc, yc)) =>
          if (ord == 0) Ordering[Int].compare(xc.value2, yc.value2)
          else ord
        }
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

  def winnings1(playHands: Seq[PlayHand]): Long = {
    val rankedHands = playHands.sortBy(_.hand)(Hand1Ordering).zipWithIndex.map { case (ph, ix) => (ph, (ix + 1)) }
//    rankedHands.map(rh => f"${rh._1.hand}  ${rh._1.bet}%4d$$  ${rh._2}%4d  |  ${rh._1.hand.toGrouppedString} ${rh._1.hand.handType1}").foreach(println)
//    rankedHands.map(rh => f"${rh._1.bet},${rh._2}").foreach(println)
    val amount = rankedHands.map { case (ph, rank) => ph.bet * rank }.sum
    amount
  }

  def winnings2(playHands: Seq[PlayHand]): Long = {
    val rankedHands = playHands.sortBy(_.hand)(Hand2Ordering).zipWithIndex.map { case (ph, ix) => (ph, (ix + 1)) }
    //    rankedHands.map(rh => f"${rh._1.hand}  ${rh._1.bet}%4d$$  ${rh._2}%4d  |  ${rh._1.hand.toGrouppedString} ${rh._1.hand.handType2}").foreach(println)
    //    rankedHands.map(rh => f"${rh._1.bet},${rh._2}").foreach(println)
    val amount = rankedHands.map { case (ph, rank) => ph.bet * rank }.sum
    amount
  }

  val sampleCards =
    """
      |32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)


  val sampleResult1 = winnings1(sampleCards.map(PlayHand.fromString))
  println(s"Sample 1: $sampleResult1") // 288

  def input = Source.fromResource("aoc2023/day07/input.txt").getLines()
  val result1 = winnings1(input.map(PlayHand.fromString).toSeq)
  println(s"Part 1: $result1") // 252656917


  val sampleResult2 = winnings2(sampleCards.map(PlayHand.fromString))
  println(s"Sample 2: $sampleResult2") // 5905

  val result2 = winnings2(input.map(PlayHand.fromString).toSeq)
  println(s"Part 2: $result2") // 253499763
}
