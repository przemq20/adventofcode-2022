package day3

import day3.UtilsDay3._
import utils.AdventSolutionApp

object Rucksacks extends AdventSolutionApp[Int] {

  def resultPart1(input: List[String]): Int = {
    val tuples = input.map(_.splitStringIntoTwoHalves())
    tuples.map(s => s._1.getSameLetter(s._2)).map(_.countPoints()).sum
  }

  def resultPart2(input: List[String]): Int = {
    val tuples = input.grouped(3).toList
    tuples.map(s => s.head.getSameLetterPart2(s(1), s(2))).map(_.countPoints()).sum
  }
}
