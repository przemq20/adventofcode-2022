package day3

import scala.annotation.tailrec

object UtilsDay3 {
  implicit class StringImprovements(val s: String) {
    def splitStringIntoTwoHalves(): (String, String) = {
      val len = s.length
      val a = s.substring(0, len / 2)
      val b = s.substring(len / 2)
      (a, b)
    }

    @tailrec
    final def getSameLetter(second: String): Char = {
      s match {
        case _ if s.isEmpty => throw new RuntimeException()
        case a@_ if second.contains(a.head) => a.head
        case _ =>
          s.drop(1).getSameLetter(second)
      }
    }

    @tailrec
    final def getSameLetterPart2(second: String, third: String): Char = {
      if (s.isEmpty) throw new RuntimeException()
      s.head match {
        case a@_ if second.contains(a) && third.contains(a) => a
        case _ => s.drop(1).getSameLetterPart2(second, third)
      }
    }
  }

  implicit class CharImprovements(val char: Char) {
    def countPoints(): Int = {
      char match {
        case c@_ if c.isLower => c.toInt - 96
        case c@_ if c.isUpper => c.toInt - 38
      }
    }
  }
}
