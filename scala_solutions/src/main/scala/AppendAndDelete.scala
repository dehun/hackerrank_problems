object AppendAndDelete {

  def solve(s:String, t:String, k:Int):Boolean = {
    lazy val endDisc = s.zip(t).dropWhile { case (l, r) => l == r}.size
    lazy val sizeDisc = Math.abs(s.size - t.size)
    lazy val rk = k - sizeDisc - endDisc*2
    (k - s.size >= t.size) || ((rk >= 0) && (rk % 2 == 0))
  }

  def main(args: Array[String]): Unit = {
    val (s, t, k) = (io.StdIn.readLine(), io.StdIn.readLine(), io.StdIn.readInt())
    if (solve(s, t, k)) {
      Console.println("Yes")
    } else {
      Console.println("No")
    }
  }
}
