package day1

import utils.AdventSolutionApp

object CalorieCounting extends AdventSolutionApp[Int] {

  override def input(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString
    finally source.close()
    lines.split("\n\n").toList
  }

  def resultPart1(input: List[String]): Int = {
    input.map(s => s.split("\n").toList.map(_.toInt).sum).max
  }

  def resultPart2(input: List[String]): Int = {
    input.map(s => s.split("\n").toList.map(_.toInt).sum).sortWith(_ > _).take(3).sum
  }
}
