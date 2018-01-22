object MissingNumbers {
  def solve(as:List[Int], bs:List[Int]):List[Int] = {
    def occur(xs:List[Int]):Map[Int, Int] = {
      var m = Map.empty[Int, Int]
      for {x <- xs} {
        m = m.updated(x, m.getOrElse(x, 0) + 1)
      }
      m
    }
    val (ao, bo) = (occur(as).toSet, occur(bs).toSet)
    (bo -- ao).map(_._1).toSet.toList.sorted
  }

  def main(args: Array[String]): Unit = {
    val an = io.StdIn.readInt()
    val as = io.StdIn.readLine().split(" ").take(an).map(_.toInt).toList
    val bn = io.StdIn.readInt()
    val bs = io.StdIn.readLine().split(" ").take(bn).map(_.toInt).toList
    Console.println(solve(as, bs).mkString(" "))
  }
}
