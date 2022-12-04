package day2

import day2.Result._
import day2.Figure._
import utils.AdventSolutionApp

object RockPaperScissors extends AdventSolutionApp[Int] {

  case class Battle(enemyFigure: Figure, userFigure: Figure)

  lazy val enemyFigures: Map[String, Figure] = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
  lazy val userFigures:  Map[String, Figure] = Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors)
  lazy val userResults:  Map[String, Result] = Map("X" -> Lose, "Y" -> Draw, "Z"  -> Win)

  def toFigures(list: List[String]): List[Battle] = {
    val pairList = list.map(a => a.split(" ").toList)
    pairList.map(battles => Battle(enemyFigures(battles.head), userFigures(battles.drop(1).head)))
  }

  def toFiguresTask2(list: List[String]): List[Battle] = {
    val pairList = list.map(str => str.split(" ").toList)
    pairList.map(
      battles =>
        Battle(enemyFigures(battles.head), userResults(battles.drop(1).head).whichFigureChoose(enemyFigures(battles.head)))
    )
  }

  def calculatePoints(list: List[Battle]): Int = {
    list
      .map(battle => battle.userFigure.winWith(battle.enemyFigure).points() + battle.userFigure.points())
      .sum
  }

  def resultPart1(input: List[String]): Int = calculatePoints(toFigures(input))
  def resultPart2(input: List[String]): Int = calculatePoints(toFiguresTask2(input))
}
