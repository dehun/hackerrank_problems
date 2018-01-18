object MakingAnagrams {
  def solve(a:String, b:String):Int = {
    def occur(x:String):Map[Char, Int] = {
      var m = Map.empty[Char, Int]
      for {c <- x} {m = m.updated(c, m.getOrElse(c, 0) + 1)}
      m
    }
    val (ao, bo) = (occur(a).toSet, occur(b).toSet)
    val diff = (ao -- bo) ++ (bo -- ao)
    diff.groupBy(_._1).map(x => (x._1, x._2.map(_._2))).map({case (c, os) =>
        if (os.size == 1) os.head
        else Math.abs(os.head - os.tail.head)
    }).sum
  }

  def main(args: Array[String]): Unit = {
    val (a, b) = (io.StdIn.readLine(), io.StdIn.readLine())
    Console.println(solve(a, b))
  }
}
