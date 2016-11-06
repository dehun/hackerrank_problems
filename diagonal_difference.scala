object Solution {
  def main(args : Array[String]) {
    val n = Console.readLine().toInt
    val rows = for (i <- 0 until n) yield Console.readLine.split(' ').map((x) => x.toInt)
    val left_diagonal = for (i <- 0 until n) yield rows(i)(i)
    val right_diagonal = for (i <- 0 until n) yield rows(i)(n - 1 - i)
    val res = (left_diagonal.sum - right_diagonal.sum).abs
    Console.println(res)
  }
}
