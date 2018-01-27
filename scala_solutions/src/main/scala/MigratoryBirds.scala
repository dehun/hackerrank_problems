object MigratoryBirds {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val xs = io.StdIn.readLine().split(" ").map(_.toInt)
    Console.println(xs.groupBy(identity).maxBy(_._2.size)._1)
  }
}
