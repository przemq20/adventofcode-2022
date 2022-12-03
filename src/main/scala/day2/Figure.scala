package day2
import Result._

sealed trait Figure {
  def points(): Int
  def winWith(figure: Figure): Result
}
object Figure {

  case object Rock extends Figure {
    override def winWith(figure: Figure): Result = {
      figure match {
        case Rock     => Draw
        case Scissors => Win
        case Paper    => Lose
      }
    }
    override def points(): Int = 1
   }

  case object Paper extends Figure {
    override def winWith(figure: Figure): Result = {
      figure match {
        case Rock     => Win
        case Scissors => Lose
        case Paper    => Draw
      }
    }
    override def points(): Int = 2

  }

  case object Scissors extends Figure {
    override def winWith(figure: Figure): Result = {
      figure match {
        case Rock     => Lose
        case Scissors => Draw
        case Paper    => Win
      }
    }
    override def points(): Int = 3
  }
}
