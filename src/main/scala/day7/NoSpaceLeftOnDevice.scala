package day7

import utils.AdventSolutionApp

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object NoSpaceLeftOnDevice extends AdventSolutionApp[Int] {

  case class File(name: String, size: Int)
  case class Directory(
    parent:        String,
    name:          String,
    directories:   ArrayBuffer[String] = ArrayBuffer.empty,
    files:         ArrayBuffer[File] = ArrayBuffer.empty,
    var totalSize: Int = 0
  )

  def parseInput(input: List[String]): mutable.Map[String, Directory] = {
    var currentDir = "/"
    val map: mutable.Map[String, Directory] = mutable.Map()
    map.addOne("/", Directory("", "/"))

    for (line <- input) {
      line match {
        case dir @ _ if dir.take(3) == "dir" =>
          val name = dir.split(" ")(1)
          map(currentDir).directories.addOne(currentDir + "/" + name)
          map.addOne(currentDir + "/" + name, Directory(currentDir, name))
        case ls @ _ if ls == "$ ls"    =>
        case cd @ _ if cd == "$ cd .." => currentDir = map(currentDir).parent
        case cd @ _ if cd.take(1) == "$" =>
          val name = cd.split(" ")(2)
          currentDir = currentDir + "/" + name
        case file @ _ =>
          val fil  = file.split(" ")
          val size = fil(0).toInt
          map(currentDir).files.addOne(File(fil(1), size))
          var fileDir = currentDir
          while (fileDir != "") {
            map(fileDir).totalSize = map(fileDir).totalSize + size
            fileDir                = map(fileDir).parent
          }
      }
    }
    map
  }

  override def resultPart1(input: List[String]): Int = {
    val map         = parseInput(input.drop(2))
    val directories = map.filter(a => a._2.totalSize < 100000)
    directories.map(_._2.totalSize).sum
  }

  override def resultPart2(input: List[String]): Int = {
    val map         = parseInput(input.drop(2))
    val unusedSpace = 70000000 - map("/").totalSize
    val spaceToFree = 30000000 - unusedSpace
    val directories = map.map(_._2.totalSize).toList.sortWith(_ < _)
    directories.filter(_ >= spaceToFree).head
  }
}
