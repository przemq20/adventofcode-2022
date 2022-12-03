package day1

import utils.AdventSolution

object CalorieCounting extends AdventSolution[Int] {

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

  def main(args: Array[String]): Unit = {
    val calories = input(inputFilePath)

    println(resultPart1(calories))
    println(resultPart2(calories))
  }
}
