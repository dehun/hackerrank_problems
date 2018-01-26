import scala.collection.mutable

object CountSort {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val xs = io.StdIn.readLine().split(" ").take(n).map(_.toInt)
    var occurs = mutable.Map.empty[Int, Int]
    for (x <- xs) {
      occurs(x) = occurs.getOrElse(x, 0) + 1
    }

    Console.println((0 until 100).map {x:Int => occurs.getOrElse(x, 0)}.mkString(" "))
  }
}
