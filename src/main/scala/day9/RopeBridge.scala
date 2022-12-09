package day9

import utils.AdventSolutionApp

object RopeBridge extends AdventSolutionApp[Int] {

  case class Order(direction: Direction, steps: Int)

  def parseInput(input: List[String]): List[Order] = {
    input.map(line => {
      val direction = Direction.toDirection(line.split(" ")(0))
      val steps     = line.split(" ")(1).toInt
      Order(direction, steps)
    })
  }

  def step(rope: Seq[Vector], direction: Vector): Seq[Vector] = {
    rope.tail
      .foldLeft(Seq(rope.head + direction)) {
        case (rope, node) =>
          val parent   = rope.head
          val distance = parent.distance(node)
          val newNode  = if (distance > 1) parent - (parent - node) / distance else node
          newNode +: rope
      }
      .reverse
  }

  def simulate(tailLength: Int, orders: List[Order]): Int = {
    orders
      .foldLeft((Seq.fill(tailLength)(Vector(0, 0)), Set(Vector(0, 0)))) {
        case ((rope, visited), order) =>
          (0 until order.steps).foldLeft((rope, visited)) {
            case ((r, visitedPoints), _) =>
              val nextStep = step(r, order.direction.toVector)
              (nextStep, visitedPoints + nextStep.last)
          }
      }
      ._2
      .size
  }

  override def resultPart1(input: List[String]): Int = {
    val commands = parseInput(input)
    simulate(2, commands)
  }

  override def resultPart2(input: List[String]): Int = {
    val commands = parseInput(input)
    simulate(10, commands)
  }
}
