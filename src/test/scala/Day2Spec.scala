import day2.RockPaperScissors
import org.scalatest.flatspec.AnyFlatSpec

class Day2Spec extends AnyFlatSpec {

  val input: List[String] = RockPaperScissors.input(RockPaperScissors.testInputFilePath)

  "Task1" should "return score when second column describes figure" in {
    val result = RockPaperScissors.resultPart1(input)
    assert(result == 15)
  }

  "Task2" should "return score when second column describes result" in {
    val result = RockPaperScissors.resultPart2(input)
    assert(result == 12)
  }

}
