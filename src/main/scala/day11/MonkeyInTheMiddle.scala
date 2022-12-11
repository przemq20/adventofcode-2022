package day11

import utils.AdventSolutionApp

import scala.collection.mutable.ArrayBuffer

object MonkeyInTheMiddle extends AdventSolutionApp[Long] {

  override def input(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString
    finally source.close()
    lines.split("\n\n").toList
  }

  def parseMonkeys(input: List[String]): List[Monkey] = {
    input.map { a =>
      val list  = a.split("\n")
      val id    = list(0).split(" ")(1).dropRight(1).toInt
      val items = list(1).split(" ").toList.drop(4).map(_.replaceAll(",", "").toInt).map(_.toLong).to(ArrayBuffer)

      def operation(old: Long): Long = {
        val aa       = list(2).split(" ").takeRight(3)
        val operator = aa.tail.head
        val second   = aa.last
        (operator, second) match {
          case ("+", "old")      => old + old
          case ("+", number @ _) => old + number.toInt
          case ("-", "old")      => old - old
          case ("-", number @ _) => old - number.toInt
          case ("*", "old")      => old * old
          case ("*", number @ _) => old * number.toInt
          case ("/", "old")      => old / old
          case ("/", number @ _) => old / number.toInt
        }
      }
      val test = list(3).split(" ").last.toInt
      def throwTo(condition: Boolean): Int = {
        val ifTrue  = list(4).split(" ").last.toInt
        val ifFalse = list(5).split(" ").last.toInt
        if (condition) ifTrue else ifFalse
      }
      Monkey(id, items, operation, test, throwTo)
    }
  }

  def inspect(monkeys: List[Monkey], iterations: Int = 20, divide: Boolean = true): List[Monkey] = {
    val lcm = monkeys.map(_.test).product
    for (_ <- 0 until iterations) {
      monkeys.foreach { monkey =>
        val items = monkey.calculateWorry(divide)
        items.foreach { item =>
          val condition = item % monkey.test == 0
          val id        = monkey.throwTo(condition)
          monkeys(id).items.addOne(item % lcm)
          monkey.count += 1
        }
        monkey.items.clear()
      }
    }
    monkeys
  }

  override def resultPart1(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    inspect(monkeys).map(_.count).sortWith(_ > _).take(2).product
  }

  override def resultPart2(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    inspect(monkeys, 10000, divide = false).map(_.count).sortWith(_ > _).take(2).product
  }
}
