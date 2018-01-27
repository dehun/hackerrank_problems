import scala.collection.Searching

object ClimbingTheLeaderboard {
  def places(scores:Array[Int]):Vector[(Int, Int)] = {
    var res = Vector.empty[(Int, Int)]
    var last = Option.empty[Int]
    var current = 1
    for {s <- scores} {
      if (!last.contains(s)) {
        res = (s, current) +: res
        current += 1
      }
      last = Some(s)
    }
    res
  }

  def solve(score:Int, places:Vector[(Int, Int)]):Int = {
    def go(ps:Vector[(Int, Int)]):Int = {
      if (ps.size == 1) {
        if (ps.head._1 > score) ps.head._2 + 1
        else if (ps.head._1 < score) ps.head._2 + 1 - 1
        else ps.head._2
      } else {
        val mid = ps(ps.size / 2)
        if (score > mid._1) {
          go(ps.splitAt(ps.size / 2)._2)
        } else if (score < mid._1) {
          go(ps.splitAt(ps.size / 2)._1)
        } else {
          mid._2
        }
      }
    }
    go(places)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val scores = io.StdIn.readLine().split(" ").map(_.toInt).take(n)
    val m = io.StdIn.readInt()
    val games = io.StdIn.readLine().split(" ").map(_.toInt).take(m)
    val ps = places(scores)
    for {g <- games} {
      Console.println(solve(g, ps))
    }
  }
}
