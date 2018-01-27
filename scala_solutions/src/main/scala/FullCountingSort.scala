object FullCountingSort {
  case class Obj(num:Int, ord:Int, value:String)
  case class ObjWrap(n:Int)(valobj:Obj)

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val xs = (1 to n)
      .map(i => (i, io.StdIn.readLine().split(" ").toList))
      .map{case (i, x::y::Nil) => new Obj(x.toInt, i, y)}.toList

    import scala.collection.mutable
    val occurs = mutable.HashMap.empty[Int, Int]
    val maps = mutable.HashMap.empty[Int, Vector[Obj]]
    for {x <- xs} {
      occurs(x.num) = occurs.getOrElse(x.num, 0) + 1
      maps(x.num) = x +: maps.getOrElse(x.num, Vector.empty)
    }

    val words = for {
      i <- 0 to 99
      w <- maps.getOrElse(i, Vector.empty).sortBy(_.ord)
    } yield { if (w.ord <= n/2) "-" else w.value}

    Console.println(words.mkString(" "))
  }
}
