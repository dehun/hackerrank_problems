object NextRound {
  def main(args: Array[String]): Unit = {
    val n::k::Nil = io.StdIn.readLine().split(" ").map(_.toInt).toList
    val xs = io.StdIn.readLine().split(" ").map(_.toInt).take(n)
    val score = xs(k - 1)
    Console.println(xs.count(s => s > 0 && s >= score))
  }
}
