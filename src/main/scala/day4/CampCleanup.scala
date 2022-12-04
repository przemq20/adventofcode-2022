package day4

import utils.AdventSolutionApp

object CampCleanup extends AdventSolutionApp[Int] {

  def toIntervalPairs(input: List[String]): List[IntervalPair] = {
    val intervals = input
      .map(_.split(",").toList.map(_.split("-").toList))
      .map(_.map(indexes => Interval(indexes(0).toInt, indexes(1).toInt)))
    intervals.map(interval => IntervalPair(interval(0), interval(1)))
  }

  override def resultPart1(input: List[String]): Int = {
    val intervalPair = toIntervalPairs(input)
    intervalPair.map(_.checkInterval()).count(a => a)
  }

  override def resultPart2(input: List[String]): Int = {
    val intervalPair = toIntervalPairs(input)
    intervalPair.map(_.overlaps()).count(a => a)
  }
}
