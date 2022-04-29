package AOC2021.AOCSolution

import scala.util.chaining.scalaUtilChainingOps

class AOC2021_3 extends AOCSolution {
  def getMostCommon(input: Array[String]): (Int,Int) = {
    var vectors: Array[List[Int]] = new Array[List[Int]](12)
    for(j <- vectors.indices) {
      vectors(j) = List()
    }
    for(token <- input) {
      for(j <- vectors.indices) {
        vectors(j) = vectors(j) :+ (if(token(j) == '1') 1 else -1)
      }
    }
    val gamma: Int = vectors
      .map { _.sum }
      .foldLeft ("") { (lhs: String, rhs: Int) => lhs + (if(rhs >= 0) "1" else "0") }
      .pipe (Integer.parseInt(_,2))
    val epsilon = (~(gamma << 20)) >>> 20
    (gamma, epsilon)
  }

  def solvePart1(input: Array[String]): String = {
    val (gamma,epsilon) = getMostCommon(input)
    (gamma * epsilon).toString
  }

  def solvePart2(input: Array[String]): String = {
    val (gamma,_) = getMostCommon(input)
    def filt(num: Int, bit: Int, op: (Int,Int)=>Boolean): Boolean = {
      val n = num >>> bit
      val g = gamma >>> bit
      val m: Boolean = op(num >>> bit,gamma >>> bit)
      if(bit == 11) println("N: " + ("0" * (12-n.toBinaryString.length)) + n.toBinaryString +
        "\nG: " + ("0" * (12-g.toBinaryString.length)) + g.toBinaryString +
        " bit: " + bit + " same: " + m)
      m
    }
    val nums: Array[Int] = input.map{Integer.parseInt(_,2)}
    var ans1: Array[Int] = Array()
    var ans2: Array[Int] = Array()
    var i = 0
    while(ans1.isEmpty) {
      ans1 = nums.filter(filt(_,i,_==_))
      i += 1
      //println(i)
      //println(filtered.mkString("Array(", ", ", ")"))
    }
    println(ans1.mkString("Array(", ", ", ")"))
    i = 0
    while(ans2.isEmpty && i<12) {
      ans2 = nums.filter(filt(_, i, (l:Int,r:Int) => ((~l << 20)>>>20)== r ))
      i += 1
    }
    println(ans2.mkString("Array(", ", ", ")"))
//    i = 0
//    while(filtered2.length != 1) {
//      filtered2 = filtered2.filter(filt(_,i,ox = false))
//      i += 1
//      //println(i)
//      //println(filtered.mkString("Array(", ", ", ")"))
//    }
//    println(filtered1.mkString(""))
//    println(filtered2.mkString(""))
//    (filtered1.head * filtered2.head).toString
    ""
  }
}
