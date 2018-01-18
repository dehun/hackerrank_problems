object ThePowerSum {
  def pows(x:Int, p:Int):List[Int] = {
    var r = List.empty[Int]
    var s = 1
    while(true) {
      val sq = Math.pow(s, p).toInt
      if (sq > x) {
        return r }
      r = sq::r
      s += 1
    }
    return r
  }

  import collection.mutable
  //var cache = mutable.Map.empty[Int, Int]
  def solve(x:Int, rest:Set[Int]):Int = { //cache.getOrElse(x, {
    if ( x <= 0 || rest.isEmpty) {
      return 0
    } else {
      var ss = if (rest.contains(x)) 1 else 0
      for {r <- rest.tails.toList.init} {
        ss += solve(x - r.head, r.tail.filter(_ <= x-r.head))
      }
      //Console.println(x, rest, ss)
      return ss
    }
  }//)

  def main(args: Array[String]): Unit = {
    val (x, n) = (io.StdIn.readInt(), io.StdIn.readInt())
    Console.println(solve(x, pows(x, n).toSet))
  }
}
