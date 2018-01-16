object WeightedUniformSubstring {
  def query(ws:Set[Int], q:Int) = ws.contains(q)
  def buildws(s:String):Set[Int] = {
    var ws = Set.empty[Int]
    val weights = ('a' to 'z').zip(1 to 26).toMap
    var pc = s.head
    var pcl = 1
    ws += weights(pc)
    for {c <- s.tail} {
      if (c == pc) {
        pcl += 1
      } else pcl = 1
      ws += pcl * weights(c)
      pc = c
    }
    ws
  }

  def main(args: Array[String]): Unit = {
    val s = io.StdIn.readLine()
    val ws = buildws(s)
    val n = io.StdIn.readInt()
    for {_ <- 1 to n} {
      val q = io.StdIn.readInt()
      if (query(ws, q)) {
        Console.println("Yes")
      } else {
        Console.println("No")
      }
    }
  }
}
