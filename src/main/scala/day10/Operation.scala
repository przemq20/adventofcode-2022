package day10

sealed trait Operation

object Operation {
  case object Noop extends Operation
  case class Addx(value: Int) extends Operation
}
