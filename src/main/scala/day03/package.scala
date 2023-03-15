package object day03 {

  private val indexAlphabet = "abcdefghijklmnopqrstuvwxyz".zipWithIndex
  private val lowerMap      = indexAlphabet.map { case (l, i) => (l, i + 1) }
  private val upperMap      = lowerMap.map { case (l, i) => (l.toUpper, 26 + i) }
  val alphaMap              = (lowerMap ++ upperMap).toMap

  def priority(char: Char): Int = alphaMap.getOrElse(char, 0)
}
