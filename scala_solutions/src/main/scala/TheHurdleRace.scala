object TheHurdleRace {
  def main(args: Array[String]): Unit = {
    val n::k::Nil = io.StdIn.readLine().split(" ").map(_.toInt).toList
    val xs = io.StdIn.readLine().split(" ").map(_.toInt).take(n)
    Console.println(Math.max(0, xs.max - k))
  }
}
