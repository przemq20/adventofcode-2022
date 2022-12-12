import day12.HillClimbingAlgorithm
import org.scalatest.flatspec.AnyFlatSpec
import utils.AdventSolutionApp
class Day12Spec extends AnyFlatSpec {

  val obj: AdventSolutionApp[_] = HillClimbingAlgorithm
  val input: List[String] = obj.input(obj.testInputFilePath)

  "Task1" should "return product of two most productive monkeys " in {
    val result = obj.resultPart1(input)
    assert(result == 31)
  }

  "Task2" should "return product of two most productive monkeys with iterations = 10000" in {
    val result = obj.resultPart2(input)
    assert(result == 29)
  }

}
