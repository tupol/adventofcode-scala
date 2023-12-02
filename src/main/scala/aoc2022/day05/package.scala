package aoc2022

import scala.collection.mutable.Stack

package object day05 {

  class Storage[T](private val stacks: Array[Stack[T]]) {

    require(stacks.size > 0, "The storage needs to have at least a stack slot")

    def show =
      stacks.zipWithIndex.foreach{ case (stack, idx) => println(s"[${idx+1}] ${stack.reverse.mkString(" ")}")}

    private def checkSlot(slot: Int) =
      require(slot >= 0 && slot < stacks.size)

    def fill(slot: Int, elements: Seq[T]): Unit =
      checkSlot(slot)
      elements.foreach(x => add(slot, x))


    def add(slot: Int, element: T): Unit =
      checkSlot(slot)
      stacks(slot).push(element)

    def move(command: MoveCommand): Unit =
      move(command.from-1, command.to-1, command.amount)

    def move(from: Int, to: Int, amount: Int): Unit =
      checkSlot(from)
      checkSlot(to)
      (0 until amount).foreach(_ => add(to, stacks(from).pop()))

    def moveBulk(command: MoveCommand): Unit =
      moveBulk(command.from - 1, command.to - 1, command.amount)
    def moveBulk(from: Int, to: Int, amount: Int): Unit =
      checkSlot(from)
      checkSlot(to)
      val tempStack = Stack.empty[T]
      (0 until amount).foreach{ _ => tempStack.push(stacks(from).pop()) }
      fill(to, tempStack.toSeq)

    def topContainers(): Seq[T] = stacks.toSeq.map(_.top)

  }

  object Storage {
    def apply[T](stacks: Int) = new Storage(Array.fill(stacks)(Stack.empty[T]))
  }

  case class MoveCommand(from: Int, to: Int, amount: Int) {
    override def toString: String = s"move $amount from $from to $to"
  }

  object MoveCommand {
    private val pattern = "move (\\d+) from (\\d+) to (\\d+)".r
    def apply(input: String): MoveCommand =
      input match
        case pattern(amount, from, to) if amount.toInt >= 0 => MoveCommand(from.toInt, to.toInt, amount.toInt)
        case str => throw new IllegalArgumentException(s"'$str' is an invalid command string")
  }
}
