import day9.RopeBridge
import org.scalatest.flatspec.AnyFlatSpec
import utils.AdventSolutionApp
class Day9Spec extends AnyFlatSpec {

  val obj: AdventSolutionApp[_] = RopeBridge
  val input: List[String] = obj.input(obj.testInputFilePath)

  "Task1" should "return number of visited points by tail when tail length =2 " in {
    val result = obj.resultPart1(input)
    assert(result == 13)
  }

  "Task2" should "return number of visited points by tail when tail length =10" in {
    val result = obj.resultPart2(input)
    assert(result == 1)
  }

}
