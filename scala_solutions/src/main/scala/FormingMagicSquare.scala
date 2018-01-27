object FormingMagicSquare {

  def p2lp(y:Int, x:Int) = y * 3 + x

  def allPossibleSquares:Stream[Array[Int]] = {
    def fillCenterV:Stream[(Array[Int], Set[Int])] = {
      val all = (1 to 9).toSet;
      {
        for {x <- all.toStream
             y <- (all - x).toStream
             z <- (all - x - y).toStream} yield {
          val arr = Array.ofDim[Int](3 * 3)
          arr(p2lp(0, 1)) = x
          arr(p2lp(1, 1)) = y
          arr(p2lp(2, 1)) = z
          (arr, all - x - y - z)
        }
      }
    }

    def fillCenterH(arr:Array[Int], left:Set[Int]):Stream[(Array[Int], Set[Int])] = {
      val magicSum = arr(p2lp(0, 1)) + arr(p2lp(1, 1)) + arr(p2lp(2, 1))
      for {x <- left.toStream
           y <- (left - x).toStream if magicSum == x + arr(p2lp(1, 1)) + y
           nextLeft = left - x - y
      } yield {
        val narr = arr.updated(p2lp(1, 0), x)
        narr(p2lp(1, 2)) = y
        (narr, nextLeft)
      }
    }

    def fillLeftCorners(arr:Array[Int], left:Set[Int]):Stream[(Array[Int], Set[Int])] = {
      val magicSum = arr(p2lp(0, 1)) + arr(p2lp(1, 1)) + arr(p2lp(2, 1))
      for {
        x <- left.toStream
        y <- (left - x).toStream if x + arr(p2lp(1, 0)) + y == magicSum
        nextLeft = left - x - y
      } yield {
        val narr = arr.updated(p2lp(0, 0), x)
        narr(p2lp(2, 0)) = y
        (narr, nextLeft)
      }
    }

    def fillRightCorners(arr:Array[Int], left:Set[Int]):Stream[Array[Int]] = {
      val magicSum = arr(p2lp(0, 1)) + arr(p2lp(1, 1)) + arr(p2lp(2, 1))
      for {
        x <- left.toStream
        y <- (left - x).toStream if (x + arr(p2lp(1, 2)) + y == magicSum) &&
          (arr(p2lp(0, 0)) + arr(p2lp(1, 1)) + y == magicSum) && (arr(p2lp(2, 0)) + arr(p2lp(1, 1)) + x == magicSum) &&
          (arr(p2lp(0, 0)) + arr(p2lp(0, 1)) + x == magicSum) && (arr(p2lp(2, 0)) + arr(p2lp(2, 1)) + y == magicSum)
      } yield {
        val narr = arr.updated(p2lp(0, 2), x)
        narr(p2lp(2, 2)) = y
        narr
      }
    }

    fillCenterV
      .flatMap({case (arr, left) => fillCenterH(arr, left)})
      .flatMap({case (arr, left) => fillLeftCorners(arr, left)})
      .flatMap({case (arr, left) => fillRightCorners(arr, left)})
  }

  def asdiff(lhs:Array[Int], rhs:Array[Int]):Int = {
    lhs.zip(rhs).map({case (l, r) => Math.abs(l - r)}).sum
  }

  def main(args: Array[String]): Unit = {
    val as = Array.ofDim[Int](3 * 3)
    for {y <- 0 until 3} {
      for {(a, x) <- io.StdIn.readLine().split(" ").map(_.toInt).zipWithIndex} {
        as(p2lp(y, x)) = a
      }
    }
    // 4 9 2  // 4 8 2  // 0 1 0
    // 3 5 7  // 4 5 7  // 1 0 0
    // 8 1 6  // 6 1 6  // 1 0 0



    Console.println(allPossibleSquares.map(ps => asdiff(ps, as)).min)
  }
}
