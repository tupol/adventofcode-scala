package aoc2022.day07

import scala.util.*

@main
def part1(): Unit = {

  import FsNode.*

  sealed trait FsNode
  object FsNode {
    case class Dir(name: String, children: Set[FsNode] = Set()) extends FsNode {
      val isRoot: Boolean = name == "/"
    }
    case class File(name: String, size: Long) extends FsNode
  }


//
//
//  enum Command:
//    case Cd(path: String)
//    case Ls
//
//  object Command {
//    private val cdPattern = """\$ cd (/)|(\.\.)|([a-zA-Z0-9_]+)"""
//    def fromString(input: String): Try[Command] =
//      case cdPattern(path) if(path == "/") => Success()
//
//  }

  def input =
    """
      |$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      |""".stripMargin

    val fs = Dir("/")

}
