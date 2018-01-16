object ExtraLongFactorials {
  def main(args: Array[String]): Unit = {
    val n:Int = io.StdIn.readInt()
    val s = (2 to n).foldLeft(BigInt(1)) { (acc:BigInt, x:Int) => acc * x }
    Console.println(s)
  }
}
