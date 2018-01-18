object TwoStrings {
  def solve(a:String, b:String):Boolean = {
    val as = a.toSet
    b.exists(as.contains(_))
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    for {_ <- 1 to n} {
      val (a, b) = (io.StdIn.readLine(), io.StdIn.readLine())
      if (solve(a, b)) {
        Console.println("YES")
      } else {
        Console.println("NO")
      }
    }
  }
}
