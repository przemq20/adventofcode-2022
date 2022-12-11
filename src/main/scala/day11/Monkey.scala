package day11

import scala.collection.mutable.ArrayBuffer

case class Monkey(
  id:        Int,
  items:     ArrayBuffer[Long],
  action:    Long => Long,
  test:      Long,
  throwTo:   Boolean => Int,
  var count: Long = 0
) {
  def calculateWorry(divide: Boolean): ArrayBuffer[Long] = {
    items.map(action(_)).map(worry => if (divide) worry / 3 else worry)
  }

}
