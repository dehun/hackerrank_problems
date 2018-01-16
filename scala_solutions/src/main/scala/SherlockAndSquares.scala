object SherlockAndSquares {
  def solve(s:Int, e:Int):Int = {
    var sq = Math.sqrt(s).ceil.toInt
    var cnt = 0
    while (true) {
      val sqq = sq * sq
      if (sqq <=e) {
        cnt += 1
      } else { return cnt }
      sq = sq+1
    }
    return cnt
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    for { _ <- 1 to n} {
      val s::e::Nil  = io.StdIn.readLine().split(" ").map(_.toInt).toList
      Console.println(solve(s, e) )
    }
  }
}
