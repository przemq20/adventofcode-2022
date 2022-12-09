import day8.TreetopTreeHouse
import org.scalatest.flatspec.AnyFlatSpec
import utils.AdventSolutionApp
class Day8Spec extends AnyFlatSpec {

  val obj: AdventSolutionApp[_] = TreetopTreeHouse
  val input: List[String] = obj.input(obj.testInputFilePath)

  "Task1" should "return number of trees visible from the grid " in {
    val result = obj.resultPart1(input)
    assert(result == 21)
  }

  "Task2" should "return product of number of visible trees from best tree" in {
    val result = obj.resultPart2(input)
    assert(result == 8)
  }

}
