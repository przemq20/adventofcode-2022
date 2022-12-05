import day5.SupplyStacks
import org.scalatest.flatspec.AnyFlatSpec

class Day5Spec extends AnyFlatSpec {

  val input: List[String] = SupplyStacks.input(SupplyStacks.testInputFilePath)

  "Task1" should "return first supply in each stack" in {
    val result = SupplyStacks.resultPart1(input)
    assert(result == "CMZ")
  }

  "Task2" should "return first supply in each stack without reversing" in {
    val result = SupplyStacks.resultPart2(input)
    assert(result == "MCD")
  }

}
