package day10

import day10.Operation._
import utils.AdventSolutionApp

import scala.annotation.tailrec
import scala.math.abs

object CathodeRayTube extends AdventSolutionApp[Any] {

  def parseInput(input: List[String]): List[Operation] = {
    input.map {
      case "noop"                             => Noop
      case addx @ _ if addx.take(4) == "addx" => Addx(addx.split(" ")(1).toInt)
    }
  }

  @tailrec
  def getLastElem[T](list: List[T]): Option[T] = {
    list match {
      case ::(head, Nil) => Some(head)
      case ::(_, tail)   => getLastElem(tail)
      case Nil           => None
    }
  }

  def execute(operations: List[Operation]): List[Int] = {
    val queue = operations.flatMap {
      case Noop        => List(0)
      case Addx(value) => List(0, value)
    }
    queue.foldLeft(List(1)) {
      case (list, elem) =>
        list ::: getLastElem(list).getOrElse(0) + elem :: Nil
    }
  }

  override def resultPart1(input: List[String]): Any = {
    val operations = parseInput(input)
    val executed   = execute(operations)
    val filtered = for {
      (x, i) <- executed.zipWithIndex
      if i % 40 == 19
    } yield x * (i + 1)
    filtered.sum
  }

  override def resultPart2(input: List[String]): Any = {
    val operations = parseInput(input)
    val executed   =  execute(operations)
    "\n" + executed
      .grouped(40)
      .map(a => a.zipWithIndex.map(elem => if (abs(elem._2 - elem._1) < 2) "###" else "   ").mkString).mkString("\n")

  }
}
