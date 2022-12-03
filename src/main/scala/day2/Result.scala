package day2

import day2.Figure._

sealed trait Result {
  def points(): Int
  def whichFigureChoose(figure: Figure): Figure

}

object Result {
  case object Win extends Result {
    override def points(): Int = 6

    override def whichFigureChoose(figure: Figure): Figure = {
      figure match {
        case Rock     => Paper
        case Paper    => Scissors
        case Scissors => Rock
      }
    }
  }

  case object Draw extends Result {
    override def points(): Int = 3

    override def whichFigureChoose(figure: Figure): Figure = figure

  }

  case object Lose extends Result {
    override def points(): Int = 0

    override def whichFigureChoose(figure: Figure): Figure = {
      figure match {
        case Rock     => Scissors
        case Paper    => Rock
        case Scissors => Paper
      }
    }
  }
}
