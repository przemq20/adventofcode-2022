import org.scalatest.flatspec.AnyFlatSpec
import day7.NoSpaceLeftOnDevice
import utils.AdventSolutionApp
class Day7Spec extends AnyFlatSpec {

  val obj: AdventSolutionApp[_] = NoSpaceLeftOnDevice
  val input: List[String] = obj.input(obj.testInputFilePath)

  "Task1" should "return size sum of smallest directories " in {
    val result = obj.resultPart1(input)
    assert(result == 95437)
  }

  "Task2" should "return size of the smallest directory that is suitable for deletion" in {
    val result = obj.resultPart2(input)
    assert(result == 24933642)
  }

}
