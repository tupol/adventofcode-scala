package object day06 {
  
  val test1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb".toCharArray.iterator
  val test2 = "bvwbjplbgvbhsrlpgdmjqwftvncz".toCharArray.iterator
  val test3 = "nppdvjthqldpwncqszvftbrmjlhg".toCharArray.iterator
  val test4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toCharArray.iterator
  val test5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray.iterator

  def firstMarker[T](input: Iterator[T], markerSize: Int) = {
    input.sliding(markerSize, 1)
      .zipWithIndex
      .dropWhile(_._1.toSet.size != markerSize)
      .nextOption()
      .map { case (xs, i) => (xs.mkString(""), i + markerSize) }
  }

}
