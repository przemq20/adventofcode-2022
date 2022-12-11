import day11.MonkeyInTheMiddle
import org.scalatest.flatspec.AnyFlatSpec
import utils.AdventSolutionApp
class Day11Spec extends AnyFlatSpec {

  val obj: AdventSolutionApp[_] = MonkeyInTheMiddle
  val input: List[String] = obj.input(obj.testInputFilePath)

  "Task1" should "return product of two most productive monkeys " in {
    val result = obj.resultPart1(input)
    assert(result == 10605)
  }

  "Task2" should "return product of two most productive monkeys with iterations = 10000" in {
    val result = obj.resultPart2(input)
    assert(result == 2713310158L)
  }

}
