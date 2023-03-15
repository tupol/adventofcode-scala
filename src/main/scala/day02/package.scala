package object day02 {

  enum Choice:
    case Rock, Paper, Scissor

  object Choice {
    def fromString(input: String): Choice =
      input.trim.toUpperCase match
        case "A" | "X" => Choice.Rock
        case "B" | "Y" => Choice.Paper
        case "C" | "Z" => Choice.Scissor
        case x => throw new IllegalArgumentException(s"$x is not acceptable; must be A or X, B or Y, C or Z")

  }

  enum Outcome:
    case Win, Loose, Draw

  object Outcome {
    def fromString(input: String): Outcome =
      input.trim.toUpperCase match
        case "X" => Outcome.Loose
        case "Y" => Outcome.Draw
        case "Z" => Outcome.Win
        case x => throw new IllegalArgumentException(s"$x is not acceptable; must be X, Y or Z")
  }


  def score(c: Choice): Int =
    c match
      case Choice.Rock => 1
      case Choice.Paper => 2
      case Choice.Scissor => 3


  def scoreA(a: Choice, b: Choice): Int =
    (a, b) match
      case (Choice.Paper, Choice.Rock) => score(a) + 6
      case (Choice.Rock, Choice.Scissor) => score(a) + 6
      case (Choice.Scissor, Choice.Paper) => score(a) + 6
      case (a, b) if a == b => score(a) + 3
      case (_, _) => score(a)

  def scoreB(a: Choice, b: Choice): Int = scoreA(b, a)

}
