object FindingDigits {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    for { _ <- 1 to n } {
      val x = io.StdIn.readInt()
      Console.println(solve(x))
    }
  }

  def solve(x:Int):Int = {
    var digits = List.empty[Int]
    var xc = Math.abs(x)
    while (xc > 0) {
      digits = (xc % 10) :: digits
      xc = xc / 10
    }
    digits.filter(d => d > 0).count(d => x % d == 0)
  }
}