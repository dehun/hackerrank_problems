object StringConstruction {
  def solve(a:String):Int = {
    var cost = 0
    var p:String = ""
    var cs = ""
    for {c <- a} {
      if (cs.nonEmpty) {
        if (p.contains(cs + c)) {
          cs = cs + c
        } else {
          p += cs
          cs = "" + c
        }
      } else {
        if (!p.contains(c)) {
          cost += 1
        } else cs = "" + c
        p = p + c
      }
    }
    cost
  }

  def main(args: Array[String]): Unit = {
    val n =io.StdIn.readInt()
    for {_ <- 1 to n} {
      Console.println(solve(io.StdIn.readLine()))
    }
  }
}
