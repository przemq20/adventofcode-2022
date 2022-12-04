package day4

case class IntervalPair(first: Interval, second: Interval) {
  def checkInterval(): Boolean = {
    if (first.left <= second.left && first.right >= second.right) true
    else if (second.left <= first.left && second.right >= first.right) true
    else false
  }

  def overlaps(): Boolean = {
    if (first.left <= second.left && first.right >= second.left) true
    else if (second.left <= first.left && second.right >= first.left) true
    else false
  }
}
