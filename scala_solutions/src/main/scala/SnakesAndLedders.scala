object SnakesAndLedders {
  def parsePair(line:String):(Int, Int) = line.split(" ").map(_.toInt).toList match {
    case x::y::Nil => (x - 1, y - 1)
    case _ => ???
  }

  def solve(ledders:Map[Int, Int], snakes:Map[Int, Int]):Int = {
    import scala.collection.mutable
    val toExpand = mutable.Queue.empty[(Int, Int)]
    toExpand.enqueue((0, 0))
    var expanded = Array.ofDim[Int](100)
    for (i <- 0 until 100) expanded(i) = Int.MaxValue

    while (toExpand.nonEmpty) {
      val expander = toExpand.dequeue()
      if (expanded(expander._1) > expander._2) {
        expanded(expander._1) = expander._2
        val expansions = (1 to 6).map(_ + expander._1).filter(_ <= 99)
        expansions.foreach { e =>
          val np = snakes.get(e).orElse(ledders.get(e)).getOrElse(e)
          toExpand.enqueue((np, expander._2 + 1))
        }
      }
    }
    if (expanded(99) == Int.MaxValue) {
      -1
    } else expanded(99)
  }

  def main(args: Array[String]): Unit = {
    val t = io.StdIn.readInt()
    for {_ <- 1 to t} {
      val n = io.StdIn.readInt()
      val ledders = for {_ <- 1 to n } yield parsePair(io.StdIn.readLine())
      val m = io.StdIn.readInt()
      val snakes = for {_ <- 1 to m } yield parsePair(io.StdIn.readLine())
      Console.println(solve(ledders.toMap, snakes.toMap))
    }
  }
}
