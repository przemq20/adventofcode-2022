import day3.Rucksacks
import org.scalatest.flatspec.AnyFlatSpec

class Day3Spec extends AnyFlatSpec {

  val input: List[String] = Rucksacks.input(Rucksacks.testInputFilePath)

  "Task1" should "return priority when one line describes two rucksacks" in {
    val result = Rucksacks.resultPart1(input)
    assert(result == 157)
  }

  "Task2" should "return priority when three lines describe three rucksacks " in {
    val result = Rucksacks.resultPart2(input)
    assert(result == 70)
  }

}
