package day9

sealed trait Direction {
  import day9.Direction._
  def toVector: Vector = {
    this match {
      case Up    => Vector(0, -1)
      case Down  => Vector(0, 1)
      case Left  => Vector(-1, 0)
      case Right => Vector(1, 0)
    }
  }

}

object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  def toDirection(string: String): Direction = {
    string match {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }
  }

}
