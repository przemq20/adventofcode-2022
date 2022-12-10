import day10.CathodeRayTube
import org.scalatest.flatspec.AnyFlatSpec
import utils.AdventSolutionApp
class Day10Spec extends AnyFlatSpec {

  val obj:   AdventSolutionApp[_] = CathodeRayTube
  val input: List[String]         = obj.input(obj.testInputFilePath)

  "Task1" should "return sum of signals " in {
    val result = obj.resultPart1(input)
    assert(result == 13140)
  }

//  "Task2" should "return number of visited points by tail when tail length =10" in {
//    val result = obj.resultPart2(input)
//    val answerPart2 =
//      """
//        |##..##..##..##..##..##..##..##..##..##..
//        |###...###...###...###...###...###...###.
//        |####....####....####....####....####....
//        |#####.....#####.....#####.....#####.....
//        |######......######......######......####
//        |#######.......#######.......#######.....""".stripMargin
//
//    assert(result == answerPart2)
//  }

}
