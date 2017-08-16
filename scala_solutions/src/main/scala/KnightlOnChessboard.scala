import scala.collection.mutable

object KnightlOnChessboard {
  def solve(n:Int, jmp:(Int, Int)):Int = {
    var mx = Array.ofDim[Option[Int]](n, n)
    var queue = new mutable.Queue[(Int, Int, Int)]()
    queue.enqueue((0, 0, 0))
    mx(0)(0) = Some(0)
    for {i <- 0 until n; j <- 0 until n} { mx(i)(j) = None }

    while (queue.nonEmpty) {
      val (jc, p1, p2) = queue.dequeue()
      val (j1, j2) = (jmp._1 + 1, jmp._2 + 1)
      val nexts = Set((p1 + j1, p2 + j2), (p1 + j1, p2 - j2), (p1 - j1, p2 + j2), (p1 - j1, p2 - j2),
                       (p1 + j2, p2 + j1), (p1 + j2, p2 - j1), (p1 - j2, p2 + j1), (p1 - j2, p2 - j1) )
      if (nexts.contains((n-1, n-1))) return 1 + jc
      nexts.filter({case (c1, c2) => c1 >= 0 && c2 >= 0 && c1 < n && c2 < n && mx(c1)(c2).isEmpty})
        .foreach(c => {
          queue.enqueue((jc + 1, c._1, c._2))
          mx(c._1)(c._2) = Some(1 + jc) })
    }

    mx(n-1)(n-1).getOrElse(-1)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    var res = Array.ofDim[Int](n-1, n-1)
    for {i <- 0 until n-1; j <- 0 until n-1} {
      if (i <= j) {
        res(i)(j) = solve(n, (i, j))
      } else {
        res(i)(j) = res(j)(i)
      }
    }
    res.foreach(r => Console.println(r.mkString(" ")))
  }
}