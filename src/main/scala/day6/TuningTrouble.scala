package day6

import utils.AdventSolutionApp

import scala.annotation.tailrec

object TuningTrouble extends AdventSolutionApp[Int] {

  @tailrec
  final def checkIfAllCharactersAreDifferent(s: String, acc: String = ""): Boolean = {
    s match {
      case _ if s.isEmpty => true
      case _ if acc.exists(c => s.contains(c)) => false
      case _ => checkIfAllCharactersAreDifferent(s.drop(1), acc + s.take(1))
    }
  }

  @tailrec
  final def markerCheck(string: String, charactersCount: Int, acc: String = "", position: Int = 0): Int = {
    string match {
      case _ if acc.length == charactersCount && checkIfAllCharactersAreDifferent(acc) => position
      case _ if acc.length < charactersCount =>
        markerCheck(string.drop(1), charactersCount, acc + string.take(1), position + 1)
      case _ => markerCheck(string.drop(1), charactersCount, acc.drop(1) + string.take(1), position + 1)
    }
  }

  override def resultPart1(input: List[String]): Int = {
    markerCheck(input.head, 4)
  }

  override def resultPart2(input: List[String]): Int = {
    markerCheck(input.head, 14)
  }
}
