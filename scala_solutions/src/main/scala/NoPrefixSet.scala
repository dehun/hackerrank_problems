import scala.annotation.tailrec
import scala.collection.mutable

object Solution {
  object Trie {
    case class Node(next:mutable.HashMap[Char, Node], var isEnd:Boolean)
    object Node {
      def emptyNode(isEnd:Boolean) = new Node(mutable.HashMap.empty, isEnd)
    }
    def emptyTrie = new Trie(Node.emptyNode(false))
  }

  class Trie(topNode:Trie.Node) {
    def add(s:String):Boolean = {
      def ensureNextNode(c:Char, node:Trie.Node) = {
        node.next.get(c) match {
          case Some(nn) => nn
          case None =>
            val nn = Trie.Node.emptyNode(false)
            node.next.update(c, nn)
            nn
        }
      }
      @tailrec
      def go(rest:List[Char], node:Trie.Node): Boolean = rest match {
        case Nil => false
        case x::Nil =>
          if (node.isEnd) return false
          val nn = ensureNextNode(x, node)
          if (nn.isEnd || nn.next.nonEmpty) false
          else {
            nn.isEnd = true
            true
          }
        case x::xs =>
          if (node.isEnd) return false
          go(xs, ensureNextNode(x, node))
      }
      go(s.toList, topNode)
    }
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val trie = Trie.emptyTrie
    for {_ <- 1 to n} {
      val s = io.StdIn.readLine()
      if (!trie.add(s)) {
        Console.println("BAD SET")
        Console.println(s)
        return
      }
    }
    Console.println("GOOD SET")
  }
}
