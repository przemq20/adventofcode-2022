package day9

case class Vector(x: Int, y: Int) {
  def +(that: Vector): Vector = Vector(x + that.x, y + that.y)
  def -(that: Vector): Vector = Vector(x - that.x, y - that.y)
  def /(v: Int): Vector = Vector(x / v, y / v)
  def distance(that: Vector): Int = Math.max(Math.abs(x - that.x), Math.abs(y - that.y))
}
