import day1.CalorieCounting
import org.scalatest.flatspec.AnyFlatSpec

class Day1Spec extends AnyFlatSpec {
  val input: List[String] = CalorieCounting.input(CalorieCounting.testInputFilePath)

  "Task1" should "return amount carried by best elf" in {
    val result = CalorieCounting.resultPart1(input)
    assert(result == 24000)
  }

  "Task2" should "return sum carried by 3 best elves" in {
    val result = CalorieCounting.resultPart2(input)
    assert(result == 45000)
  }
}
