object CatsAndMouse {
  def main(args: Array[String]): Unit = {
    val t = io.StdIn.readInt()
    for {_ <- 1 to t} {
      val (x::y::z::Nil) = io.StdIn.readLine().split(" ").map(_.toInt).toList
      if (Math.abs(x - z) == Math.abs(y - z)) Console.println("Mouse C")
      else if (Math.abs(x - z) < Math.abs(y - z)) { Console.println("Cat A")}
      else Console.println("Cat B")
    }
  }
}
