package AOC2021.AOCSolution

class AOC2018_1 extends AOCSolution {
  override def solvePart1(input: Array[String]): String = {
    val nums = input
    nums.map(Integer.valueOf).foldLeft(0)(_+_).toString
  }

  override def solvePart2(input: Array[String]): String = {
    val nums = input.map(Integer.valueOf)
    var vals = List[Int](0)
    var i: Int = 0
    while (true) {
      println(i)
      i += 1
      for (num <- nums) {
        //println(vals)
        vals = vals :+ vals.last + num
        if (vals.init contains vals.last) return vals.last.toString
      }
    }
    "No match found"
  }
}
