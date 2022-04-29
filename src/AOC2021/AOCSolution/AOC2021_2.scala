package AOC2021.AOCSolution

import scala.util.matching.Regex

class AOC2021_2 extends AOCSolution {
  override def solvePart1(input: Array[String]): String = {
    val up = raw"up ([0-9]+)".r
    val down = raw"down ([0-9]+)".r
    val forward = raw"forward ([0-9]+)".r
    val categorized = input map {
      case forward(x) => (x.toInt,0)
      case down(x) => (x.toInt,1)
      case up(x) => (x.toInt,2)
    } groupBy {
      case (_,0) => 0
      case (_,1) => 1
      case (_,2) => 2
    }
    (categorized(0).map(_._1).sum * (categorized(1).map(_._1).sum - categorized(2).map(_._1).sum)).toString
  }

  def solvePart2Old(input: Array[String]): String = {
    val up = raw"up ([0-9]+)".r
    val down = raw"down ([0-9]+)".r
    val forward = raw"forward ([0-9]+)".r
    val command = raw"[a-z]+ ([0-9]+)".r
    val t = input.mkString("\n").split("(?<=forward [0-9]+)") // Separate into tokens like (c1\nc2\n...\nforward x)
      .map { // For each token...
        _.split("\n").filterNot(_.isEmpty) // Separate into tokens like (command x)
          .groupMap {
            case forward(_) => 0
            case down(_) => 1
            case up(_) => 1
          } {
            case forward(x) => x.toInt
            case down(x) => x.toInt
            case up(x) => -x.toInt
          }
          .map {
            case (k,v) => (k,v.sum)
          }
      } .foldLeft ((0,0,0)) ( (c,m) => { // dist,depth,aim. m = Map[0 - multiplier, 1 - aim_delta]
      (c._1+m(0),c._2+(c._3+m.getOrElse(1,0))*m(0),c._3+m.getOrElse(1,0))
    })
    (t._1*t._2).toString
  }

  def solvePart2(input: Array[String]): String = {
    val up = raw"up ([0-9]+)".r
    val notup = raw"(?:down|forward) ([0-9]+)".r
    val rv = input.mkString("\n").split("(?<=forward [0-9]+\n)") // separate into blocks before each "forward"
      .map { tok => // from each block, we need total change in aim and X of forward X
          val l = tok.split("\n").map{ // capture X from each command
            case up(x) => -x.toInt
            case notup(x) => x.toInt
          } // return (vertical_delta, forward)
          (if(l.length != 1) l.init.sum else 0, l.last) // special case for consecutive forwards!
      }
      .foldLeft ((0,0,0)) ( (c,n) => { // starting from all 0s, accumulate (distance,depth,aim)
      (c._1+n._2, c._2+(c._3+n._1)*n._2, c._3+n._1)
    })
    (rv._1*rv._2).toString // rv is the final state of submarine (distance,depth,aim) after folding
  }
}
