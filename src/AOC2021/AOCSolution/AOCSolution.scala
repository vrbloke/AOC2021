package AOC2021.AOCSolution

import java.io.File
import java.util.Scanner

abstract class AOCSolution {
  def solve(part: Int, input: Array[String]): String = {
    part match {
      case 1 => solvePart1(input)
      case 2 => solvePart2(input)
      case _ => "Incorrect part number"
    }
  }

  def solvePart1(input: Array[String]): String
  def solvePart2(input: Array[String]): String

  def scanInput(filename: String): String = {
    val sc: Scanner = new Scanner(new File(filename)).useDelimiter("\n")
    val buf: StringBuffer = new StringBuffer()
    while(sc.hasNext) {
      buf.append(sc.next()+"\n")
    }
    return buf.toString
  }

  def arrayInput(filename: String): Array[String] = scanInput(filename).split('\n')

  def arrayizeString(input: String): Array[String] = input.split('\n')

  def solveFile(part: Int, filename: String): String = {
    solve(part, arrayInput(filename))
  }

  def solveString(part: Int, input: String): String = {
    solve(part, arrayizeString(input))
  }

  def solveFileTimed(part: Int, filename: String): String = {
    val input = arrayInput(filename)
    val t0 = System.nanoTime()
    val rv = solve(part, input)
    val t1 = System.nanoTime()
    println("Time elapsed: " + (t1-t0))
    rv
  }
}
