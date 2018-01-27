object DesignedPdfViewer {
  def main(args: Array[String]): Unit = {
    val heights = ('a' to 'z').zip(io.StdIn.readLine().split(" ").map(_.toInt)).toMap
    val word = io.StdIn.readLine()
    val r = word.map(heights(_)).max * word.size
    Console.println(r)
  }
}
