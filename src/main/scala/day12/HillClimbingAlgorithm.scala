package day12

import utils.AdventSolutionApp

import scala.collection.mutable

object HillClimbingAlgorithm extends AdventSolutionApp[Int] {

  case class Board(list: List[List[Point]]) {
    def checkPoints(point1: (Int, Int), point2: (Int, Int)): Boolean =
      if (list(point1._1)(point1._2).height - list(point2._1)(point2._2).height <= 1)
        true
      else false
  }

  case class Point(
    height:       Int,
    isStart:      Boolean = false,
    isEnd:        Boolean = false,
    var visited:  Boolean = false,
    var distance: Int = 1000,
    var prev:     Option[(Int, Int)] = None
  ) {
    def notVisited: Boolean = !visited
  }

//  override val inputFilePath: String = testInputFilePath

  def parseInput(input: List[String]): List[List[Point]] = {
    val length    = input.map(_.length).head
    val firstLine = for (_ <- 0 until length + 2) yield Point(1000)
    firstLine.toList :: input
      .map(
        line =>
          line.map {
            case 'S'      => Point('a'.toInt - 97, isStart = true)
            case 'E'      => Point('z'.toInt - 97, isEnd = true)
            case char @ _ => Point(char.toInt - 97)
          }.toList
      )
      .map(line => Point(1000) :: line ::: Point(1000) :: Nil) ::: firstLine.toList :: Nil
  }

  def bfs(board: Board): Int = {
    val queue: mutable.Queue[(Int, Int, Int)] = mutable.Queue((1, 1, 0))
    var shortest = Int.MaxValue
    board.list(1)(1).visited = true
    while (queue.nonEmpty) {
      val elem     = queue.dequeue()
      val x        = elem._1
      val y        = elem._2
      val distance = elem._3
      if (board.list(x)(y).isEnd) {
        if (distance < shortest) {
          shortest = distance
        }
        return distance
      }
      if (board.checkPoints((x - 1, y), (x, y)) && !board.list(x - 1)(y).visited) {
        board.list(x - 1)(y).visited = true
        queue.enqueue((x - 1, y, distance + 1))
      }
      if (board.checkPoints((x, y + 1), (x, y)) && !board.list(x)(y + 1).visited) {
        board.list(x)(y + 1).visited = true
        queue.enqueue((x, y + 1, distance + 1))
      }
      if (board.checkPoints((x + 1, y), (x, y)) && !board.list(x + 1)(y).visited) {
        board.list(x + 1)(y).visited = true
        queue.enqueue((x + 1, y, distance + 1))
      }
      if (board.checkPoints((x, y - 1), (x, y)) && !board.list(x)(y - 1).visited) {
        board.list(x)(y - 1).visited = true
        queue.enqueue((x, y - 1, distance + 1))
      }
    }
    shortest
  }

  var shortestRec = Int.MaxValue
  def recursiveSearch(board: Board, currPoint: (Int, Int), distance: Int = 0): Unit = {
    val x = currPoint._1
    val y = currPoint._2
//    println(x, y)
    board.list(x)(y).visited = true
    if (board.list(x)(y).isEnd) {
      println(distance)
      if (distance < shortestRec) shortestRec = distance
    }
    if (board.checkPoints((x - 1, y), (x, y))) {
      board.list(x-1)(y).visited = true
      recursiveSearch(board, (x - 1, y), distance + 1)
      board.list(x-1)(y).visited = false
    }
    if (board.checkPoints((x, y + 1), (x, y))) {
      board.list(x)(y+1).visited = true
      recursiveSearch(board, (x, y + 1), distance + 1)
      board.list(x)(y+1).visited = false
    }
    if (board.checkPoints((x + 1, y), (x, y))) {
      board.list(x+1)(y).visited = true
      recursiveSearch(board, (x + 1, y), distance + 1)
      board.list(x+1)(y).visited = false

    }
    if (board.checkPoints((x, y - 1), (x, y))) {
      board.list(x)(y-1).visited = true
      recursiveSearch(board, (x, y - 1), distance + 1)
      board.list(x)(y-1).visited = false
    }
    board.list(x)(y).visited = false
  }

  def dijkstra(board: Board, start: (Int, Int) = (1, 1)): Board = {
    val queue: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer()
    for (i <- board.list.indices) {
      for (j <- board.list(i).indices) {
        board.list(i)(j).distance = 1000
        board.list(i)(j).prev     = None
        queue.addOne((i, j))
      }
    }
    board.list(start._1)(start._2).distance = 0
    while (queue.nonEmpty) {
      val sorted = queue.sortWith((a, b) => board.list(a._1)(a._2).distance < board.list(b._1)(b._2).distance)
      val u = sorted(0)
      sorted.remove(0)
      queue.clear()
      queue.addAll(sorted)
      if (queue.contains((u._1 + 1, u._2)) && board.checkPoints((u._1 + 1, u._2), (u._1, u._2))) {
        val alt = board.list(u._1)(u._2).distance + 1
        if (alt < board.list(u._1 + 1)(u._2).distance) {
          board.list(u._1 + 1)(u._2).distance = board.list(u._1)(u._2).distance + 1
          board.list(u._1 + 1)(u._2).prev     = Some(u._1, u._2)
        }
      }
      if (queue.contains((u._1 - 1, u._2)) && board.checkPoints((u._1 - 1, u._2), (u._1, u._2))) {
        val alt = board.list(u._1)(u._2).distance + 1
        if (alt < board.list(u._1 - 1)(u._2).distance) {
          board.list(u._1 - 1)(u._2).distance = board.list(u._1)(u._2).distance + 1
          board.list(u._1 - 1)(u._2).prev     = Some(u._1, u._2)
        }
      }
      if (queue.contains((u._1, u._2 + 1)) && board.checkPoints((u._1, u._2 + 1), (u._1, u._2))) {
        val alt = board.list(u._1)(u._2).distance + 1
        if (alt < board.list(u._1)(u._2 + 1).distance) {
          board.list(u._1)(u._2 + 1).distance = board.list(u._1)(u._2).distance + 1
          board.list(u._1)(u._2 + 1).prev     = Some(u._1, u._2)
        }
      }
      if (queue.contains((u._1, u._2 - 1)) && board.checkPoints((u._1, u._2 - 1), (u._1, u._2))) {
        val alt = board.list(u._1)(u._2).distance + 1
        if (alt < board.list(u._1)(u._2 - 1).distance) {
          board.list(u._1)(u._2 - 1).distance = board.list(u._1)(u._2).distance + 1
          board.list(u._1)(u._2 - 1).prev     = Some(u._1, u._2)
        }
      }
    }
    board
  }

  override def resultPart1(input: List[String]): Int = {
    val board = Board(parseInput(input))
    dijkstra(board, (1, 1)).list.map(_.filter(_.isEnd)).filter(_.nonEmpty).head.head.distance
  }

  override def resultPart2(input: List[String]): Int = {
    var shortest = Int.MaxValue
    val board    = Board(parseInput(input))
    val lowestPoints: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer()
    for (i <- board.list.indices) {
      for (j <- board.list(i).indices) {
        if (board.list(i)(j).height == 0) lowestPoints.addOne((i, j))
      }
    }
    println(lowestPoints.length)
    lowestPoints.zipWithIndex.foreach(points => {
      println(s"${points._2}/${lowestPoints.length}")
      val res = dijkstra(board, points._1).list.map(_.filter(_.isEnd)).filter(_.nonEmpty).head.head.distance
      if (res < shortest) shortest = res
    })
      shortest
  }
}
