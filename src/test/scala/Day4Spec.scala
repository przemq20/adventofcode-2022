import day4.CampCleanup
import org.scalatest.flatspec.AnyFlatSpec

class Day4Spec extends AnyFlatSpec {

  val input: List[String] = CampCleanup.input(CampCleanup.testInputFilePath)

  "Task1" should "return number of fully contains intervals" in {
    val result = CampCleanup.resultPart1(input)
    assert(result == 2)
  }

  "Task2" should "return number of overlapping intervals " in {
    val result = CampCleanup.resultPart2(input)
    assert(result == 4)
  }

}
