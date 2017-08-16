object ClosestNumbers {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val xs = io.StdIn.readLine().split(" ").map(_.toInt)
    val ss = xs.sorted
    var res = List.empty[(Int, Int)]
    def pd(p:(Int, Int)) = Math.abs(p._2 - p._1)
    for {p <- ss.init.zip(ss.tail) } {
      if (res.nonEmpty) {
        if (pd(res(0)) == pd(p)) { res = p::res }
        else if (pd(res(0)) > pd(p)) { res = List(p)}
      } else { res = List(p) }
    }
    Console.println(res.flatMap(p => List(p._1, p._2)).sorted.mkString(" "))
  }
}
