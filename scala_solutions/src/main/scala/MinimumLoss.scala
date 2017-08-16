import java.util

import scala.collection.{SortedSet, mutable}
import java.util.TreeSet


object Solution {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val ps = io.StdIn.readLine().split(" ").map(_.toLong).toList
    val rs = new util.TreeSet[Long]
    for {p <- ps} rs.add(p)

    var mm:Option[Long] = None
    for {p <- ps.init} {
      rs.remove(p)
      val lb = Some(rs.floor(p)).filter(x => x != 0 && x < p)
      val ub = Some(rs.ceiling(p)).filter(x => x != 0 && x < p)
      val ms = List(lb, ub).map(_.map(x => Math.abs(p - x))).filter(_.isDefined).map(_.get)
      if (ms.nonEmpty) {
        mm = mm.map(_.min(ms.min)).orElse(Some(ms.min))
      }
    }

    Console.println(mm.get)
  }
}
