object Watermelon {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    if (n % 2 == 0) Console.println("YES") else Console.println("NO")
  }
}
