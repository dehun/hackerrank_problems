import CrossWord.{Input, Position}

object CrossWord {
  case class Position(y:Int, x:Int)
  case class Input(at:Position, end:Position) {
    def isCrossing(other:Input):Boolean = {
      if (at.y == end.y) { // we are horizontal
        val oury = at.y
        val otherx = other.at.x
        (other.at.x ==other.end.x) && (otherx >=  at.x && otherx <= end.x) && (other.at.y <= oury && other.end.y >= oury)
      } else { // we are vertical
        val ourx = at.x
        val othery = other.at.y
        (other.at.y == other.end.y) && (othery >= at.y && othery <= end.y) && (other.at.x <= ourx && other.end.x >= ourx)
      }
    }

    def toPositions:List[Position] = {
      if (at.y == end.y) {
        (for {x <- at.x to end.x} yield Position(at.y, x)) toList
      } else {
        (for {y <- at.y to end.y} yield Position(y, at.x)) toList
      }
    }

    lazy val size:Int = Math.max(end.x - at.x + 1, end.y - at.y + 1)
  }

  def fromMatrix(arr: Vector[Vector[Char]]): CrossWord = {
    var starts = Set.empty[Input]

    // vertical
    for {x <- 0 until 10} {
      var firstBox: Option[Position] = Option.empty
      for {y <- 0 until 10} {
        if (firstBox.isDefined) {
          if (arr(y)(x) == '-') { }
          else {
            starts += Input(firstBox.get, Position(y - 1, x))
            firstBox = None
          }
        } else {
          if (arr(y)(x) == '-') {
            firstBox = Some(Position(y, x))
          }
        }
      }
      if (firstBox.isDefined) { starts += Input(firstBox.get, Position(9, x))}
    }

    // horizontal
    for {y <- 0 until 10} {
      var firstBox: Option[Position] = Option.empty
      for {x <- 0 until 10} {
        if (firstBox.isDefined) {
          if (arr(y)(x) == '-') { }
          else {
            starts += Input(firstBox.get, Position(y, x-1))
            firstBox = None
          }
        } else {
          if (arr(y)(x) == '-') {
            firstBox = Some(Position(y, x))
          }
        }
      }
      if (firstBox.isDefined) { starts += Input(firstBox.get, Position(y, 9))}
    }

    new CrossWord(starts)
  }
}

case class FilledInput(input: Input, filledIn:String) {
  def resolve(p:Position):Option[Char] = {
    if (input.at.x == input.end.x && input.at.x == p.x) {        // vertical
      if (input.at.y <= p.y) {
        Some(filledIn(p.y - input.at.y))
      } else None
    } else if (input.at.y == input.end.y && input.at.y == p.y) { // horizontal
      if (input.at.x <= p.x) {
        Some(filledIn(p.x - input.at.x))
      } else None
    } else None
  }
}

case class HalfSolvedCrossword(inputsLeft:Set[Input], filledInputs:Map[Input, FilledInput])
case class SolvedCrossword(filledInput: Set[FilledInput]) {
  def toMatrix:Vector[Vector[Char]] = {
    val r = Array.ofDim[Char](10, 10)
    for {y <- 0 until 10
         x <- 0 until 10} { r(y)(x) = '+'}
    for {i <- filledInput} {
      for {p <- i.input.toPositions.zip(i.filledIn) } {
        r(p._1.y)(p._1.x) = p._2
      }
    }
    r.map(_.toVector).toVector
  }
}

class CrossWord(inputs:Set[Input]) {
  lazy val crossings = {
    for { i <- inputs } yield {
      (i, inputs.filter(_.isCrossing(i)))
    }
  }.toMap

  def solve(words:Vector[String]):Option[SolvedCrossword]= {
    def go(wordsLeft:Vector[String], hc:HalfSolvedCrossword):Option[SolvedCrossword] = {
      if (wordsLeft.isEmpty || hc.inputsLeft.isEmpty) return Some(SolvedCrossword(hc.filledInputs.values.toSet))
      val fillingWord = wordsLeft.head
      val possibleInputs = hc.inputsLeft
        .filter(_.size == fillingWord.size)
        .filter({i =>
          val fi = FilledInput(i, fillingWord)
          if (crossings.contains(i)) {
            val cis = crossings(i).filter{ci => hc.filledInputs.contains(ci)}
            cis.forall{ ci =>
              val cif = hc.filledInputs(ci)
              val commonPositions = i.toPositions.toSet & cif.input.toPositions.toSet
              commonPositions.forall { cp => fi.resolve(cp) == cif.resolve(cp) }
            }
          } else true
        })
      possibleInputs.toStream.map({i =>
        val filledInput = FilledInput(i, fillingWord)
        val newFilledInputs = hc.filledInputs.updated(i, filledInput)
        val newHc = HalfSolvedCrossword(hc.inputsLeft - i, newFilledInputs)
        go(wordsLeft.tail, newHc)
      }).find(_.isDefined).flatten
    }
    go(words, HalfSolvedCrossword(inputs, Map.empty))
  }
}

object CrosswordPuzzle {
  def solve(arr:Vector[Vector[Char]], words:Vector[String]):Vector[Vector[Char]] = {
    CrossWord.fromMatrix(arr).solve(words).get.toMatrix
  }

  def main(args: Array[String]): Unit = {
    val arr:Vector[Vector[Char]] = {
      for {_ <- 1 to 10} yield {
        io.StdIn.readLine().toVector
      }
    }.toVector
    val words = io.StdIn.readLine().split(";").toVector
    Console.println(solve(arr, words).map(_.mkString("")).mkString("\n"))
  }
}
