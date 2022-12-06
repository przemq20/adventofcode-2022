import org.scalatest.flatspec.AnyFlatSpec
import day6.TuningTrouble
class Day6Spec extends AnyFlatSpec {

  val input: List[String] = TuningTrouble.input(TuningTrouble.testInputFilePath)

  "checkIfAllCharactersAreDifferent" should "return true if all characters in string are different" in {
    assert(TuningTrouble.checkIfAllCharactersAreDifferent("asdvc"))
  }

  it should "return false if all characters in string are the same" in {
    assert(!TuningTrouble.checkIfAllCharactersAreDifferent("aaaaa"))
  }

  "Task1" should "return position of marker" in {
    val result = TuningTrouble.resultPart1(input)
    assert(result == 7)
  }

  "Task2" should "return position of message" in {
    val result = TuningTrouble.resultPart2(input)
    assert(result == 19)
  }

}
