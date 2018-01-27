object ElectronicShop {
  def main(args: Array[String]): Unit = {
    val s::n::m::Nil = io.StdIn.readLine().split(" ").map(_.toInt).take(3).toList
    val xs = io.StdIn.readLine().split(" ").map(_.toInt).take(n).toList
    val ys = io.StdIn.readLine().split(" ").map(_.toInt).take(m).toList
    val allPairs = for {x <- xs;  y <- ys} yield { x + y }
    val pair = allPairs.toStream.filter{sm => s >= sm}
    if (pair.isEmpty) Console.println(-1)
    else Console.println(pair.max)
  }
}
