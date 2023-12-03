package aoc2023.day02

import scala.io.Source

object main extends App {

  case class GameTurn(red: Int, green: Int, blue: Int)
  object GameTurn {
    def fromString(input: String): GameTurn = {
      val rx = input.split(",").map{ kv =>
        val px = kv.trim.split(" ")
        if(px.size == 2) then (px(1), px(0).toInt) else ("", 0)
      }.toMap
      GameTurn(
        red = rx.get("red").getOrElse(0),
        green = rx.get("green").getOrElse(0),
        blue = rx.get("blue").getOrElse(0)
      )
    }
  }

  case class Game(id: Int, turns: Seq[GameTurn])
  object Game{
    def fromString(input: String): Game = {
      val px = input.split(":")
      val id = px(0).toLowerCase.replace("game", "").trim.toInt
      val turns = px(1).split(";").map(GameTurn.fromString)
      Game(id, turns)
    }
  }

  object GameRules {
    class MaxPerTurn(red: Int, green: Int, blue: Int) {
      def checkTurn(turn: GameTurn): Boolean =
        turn.red <= red && turn.blue <= blue && turn.green <= green
      def checkGame(game: Game): Boolean =
        game.turns.map(checkTurn).reduce(_ && _)
    }
    object MaxPerGame {
      def max(turn1: GameTurn, turn2: GameTurn): GameTurn =
        GameTurn(
          red = math.max(turn1.red, turn2.red),
          green = math.max(turn1.green, turn2.green),
          blue = math.max(turn1.blue, turn2.blue)
        )
      def checkGame(game: Game): GameTurn =
        game.turns.reduce((t1, t2) => max(t1, t2))

      def gamePower(game: Game): Int = {
        val t = checkGame(game)
        t.red * t.green * t.blue
      }
    }
  }

  val sampleInput: Seq[String] =
    """
      |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      |""".stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

  val maxPerTurn = GameRules.MaxPerTurn(12, 13, 14)

  val sampleGames: Seq[Game] = sampleInput.map(Game.fromString(_))
  val sampleResult = sampleGames.map(g => (g, maxPerTurn.checkGame(g))).filter(_._2).map(_._1.id).reduce(_ + _)
  println(sampleResult)

  def games = Source.fromResource("aoc2023/day02/input1.txt").getLines().map(Game.fromString(_))
  val result = games.map(g => (g, maxPerTurn.checkGame(g))).filter(_._2).map(_._1.id).reduce(_ + _)
  println(result)

  val result2 = games.map(GameRules.MaxPerGame.gamePower).reduce(_ + _)
  println(result2)
}
