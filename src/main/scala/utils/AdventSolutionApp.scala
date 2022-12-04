package utils

trait AdventSolutionApp[T] {
  val inputFilePath:     String = s"src/main/resources/${getClass.getPackageName}/input.txt"
  val testInputFilePath: String = s"src/main/resources/${getClass.getPackageName}/test-input.txt"

  def input(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString
    finally source.close()
    lines.split("\n").toList
  }

  def resultPart1(input: List[String]): T

  def resultPart2(input: List[String]): T

  def main(args: Array[String]): Unit = {
    val lines = input(inputFilePath)
    println(resultPart1(lines))
    println(resultPart2(lines))
  }
}
