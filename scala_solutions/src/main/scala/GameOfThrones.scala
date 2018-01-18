object GameOfThrones {
  def solve(s:String): Boolean = {
    def occur(x:String):Set[(Char, Int)] = {
      var m = Map.empty[Char, Int]
      for {c <- x} {
        m = m.updated(c, m.getOrElse(c, 0) + 1)
      }
      m.toSet
    }
    occur(s).count(_._2 % 2 != 0) <= 1
  }
  def main(args: Array[String]): Unit = {
    if(solve(io.StdIn.readLine())) {
      Console.println("YES")
    } else {
      Console.println("NO")
    }
  }
}
