package day8

import utils.AdventSolutionApp
import scala.util.control.Breaks._

object TreetopTreeHouse extends AdventSolutionApp[Int] {

  case class Tree(height: Int, var isVisible: Boolean = false)

  def parseInput(input: List[String]): List[List[Tree]] = {
    input.map(_.map(c => Tree(c.asDigit)).toList)
  }

  def checkForestPart1(forest: List[List[Tree]]): List[List[Tree]] = {
    val width  = forest.head.length
    val height = forest.length
    forest.head.foreach(_.isVisible        = true)
    forest(height - 1).foreach(_.isVisible = true)
    for (i <- 0 until height) {
      forest(i).head.isVisible       = true
      forest(i)(width - 1).isVisible = true
    }

    var change = true
    while (change) {
      change = false
      for (i <- 1 until height - 1) {
        for (j <- 1 until width - 1) {
          var curr = -1
          for (k <- 0 to i) {
            if (forest(k)(j).height > curr) {
              forest(k)(j).isVisible = true
              curr                   = forest(k)(j).height
            }
          }
          curr = -1
          for (k <- 0 to j) {
            if (forest(i)(k).height > curr) {
              forest(i)(k).isVisible = true
              curr                   = forest(i)(k).height
            }
          }
          curr = -1
          for (k <- height - 1 to i by -1) {
            if (forest(k)(j).height > curr) {
              forest(k)(j).isVisible = true
              curr                   = forest(k)(j).height
            }
          }
          curr = -1
          for (k <- height - 1 to j by -1) {
            if (forest(i)(k).height > curr) {
              forest(i)(k).isVisible = true
              curr                   = forest(i)(k).height
            }
          }
        }
      }
    }
    forest
  }

  def checkForestPart2(forest: List[List[TreetopTreeHouse.Tree]]): Int = {
    val width  = forest.head.length
    val height = forest.length
    var max    = 0
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        var a, b, c, d: Int = 0
        val treeHeight = forest(i)(j).height
        breakable {
          for (k <- i + 1 until height) {
            a += 1
            if (forest(k)(j).height < treeHeight) {} else {
              break()
            }
          }
        }
        breakable {
          for (k <- j + 1 until width) {
            b += 1
            if (forest(i)(k).height < treeHeight) {} else {
              break()
            }
          }
        }
        breakable {
          for (k <- j - 1 to 0 by -1) {
            c += 1
            if (forest(i)(k).height < treeHeight) {} else {
              break()
            }
          }
        }
        breakable {
          for (k <- i - 1 to 0 by -1) {
            d += 1
            if (forest(k)(j).height < treeHeight) {} else {
              break()
            }
          }
        }
        if (a * b * c * d > max) {
          max = a * b * c * d
        }
      }
    }
    max
  }

  override def resultPart1(input: List[String]): Int = {
    val forest = parseInput(input)
    val f      = checkForestPart1(forest)
    f.flatMap(_.map(_.isVisible)).count(a => a)

  }

  override def resultPart2(input: List[String]): Int = {
    val forest = parseInput(input)
    val f      = checkForestPart2(forest)
    f
  }
}
