package day5

import utils.AdventSolutionApp

import scala.annotation.tailrec

object SupplyStacks extends AdventSolutionApp[String] {

  case class Supply(item:  Char)
  case class Stack(number: Int, items: List[Supply])

  override def input(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString
    finally source.close()
    lines.split("\n\n").toList
  }

  def parseOrders(str: String): List[Order] = {
    val lines = str.split("\n")
    val list  = lines.map(_.split(" "))
    list.map(line => Order(line(1).toInt, line(3).toInt, line(5).toInt)).toList
  }

  @tailrec
  final def getLetters(string: String, index: Int = 0, result: String = ""): String = {
    string match {
      case _ if string.isEmpty => result
      case _ if index == 1     => getLetters(string.drop(1), (index + 1) % 4, result + string.head)
      case _                   => getLetters(string.drop(1), (index + 1) % 4, result)
    }
  }

  def toSupplyStack(supplies: List[List[Supply]], index: Int): Stack = {
    Stack(index, supplies.map(_(index - 1)).filterNot(_.item == ' '))
  }

  def toSupplyStacks(supplies: List[List[Supply]], quantity: Int): List[Stack] = {
    List.range(1, quantity + 1).map(index => toSupplyStack(supplies, index))
  }

  def parseStacks(str: String): List[Stack] = {
    val stackQuantity = str(str.length - 1).toInt - 48
    val lines = str.split("\n").toList.dropRight(1).map { line =>
      val length = stackQuantity * 3 + stackQuantity - 1
      line match {
        case _ if line.length == length => line
        case _ =>
          val emptyString = List.range(0, length - line.length).map(_ => " ").mkString("")
          line + emptyString
      }
    }
    val supplies = lines.map(str => getLetters(str)).map(_.map(char => Supply(char)).toList)
    toSupplyStacks(supplies, stackQuantity)
  }

  @tailrec
  final def executeOrders(orders: List[Order], stacks: List[Stack], reverse: Boolean = true): List[Stack] = {
    orders match {
      case Nil => stacks
      case ::(head, next) =>
        executeOrders(next, head.executeOrder(stacks, reverse), reverse)
    }
  }

  def getOrders(input: List[String]): List[Order] = {
    val ordersStr = input(1)
    parseOrders(ordersStr)
  }

  def getStacks(input: List[String]): List[Stack] = {
    val stacksStr = input(0)
    parseStacks(stacksStr)
  }

  def getResults(input: List[String], reverse: Boolean): String = {
    val orders   = getOrders(input)
    val stacks   = getStacks(input)
    val executed = executeOrders(orders, stacks, reverse)
    executed.map(_.items.headOption.getOrElse(Supply(' ')).item).mkString("")
  }

  override def resultPart1(input: List[String]): String = {
    getResults(input, reverse = true)
  }
  override def resultPart2(input: List[String]): String = {
    getResults(input, reverse = false)
  }
}
