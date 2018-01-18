object Anagram {
  def solve(s:String):Int = {
    if (s.size % 2 != 0) return -1
    val (l, r) = s.splitAt(s.size/2)
    def occurs(x:String):Map[Char, Int] = {
      var m = Map.empty[Char, Int]
      for {c <- x} {
        m = m + (c -> (m.getOrElse(c, 0) + 1))
      }
      m
    }
    val (lo, ro) = (occurs(l), occurs(r))
    val (lc, rc) = (lo.toSet, ro.toSet)
    val inl = rc -- lc
    val inr = lc -- rc
    val lt = inl.map(_._1).toList.map{x =>
      val xl = lo.getOrElse(x, 0)
      val xr = ro.getOrElse(x, 0)
      if ( xl > xr) 0 else {
        xr - xl }
    }.sum
    val rt = inr.map(_._1).toList.map{x =>
      val xl = ro.getOrElse(x, 0)
      val xr = lo.getOrElse(x, 0)
      if ( xl > xr) 0 else xr - xl
    }.sum

    Math.min(lt, rt)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    for {_ <- 1 to n } {
      Console.println(solve(io.StdIn.readLine()))
    }
  }
}
