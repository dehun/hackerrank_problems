import scala.annotation.tailrec
import scala.collection.mutable

object ContactsList {
  sealed trait Command
  case class Add(name:String) extends Command
  case class Find(part:String) extends Command

  object Command {
    def parseCommand(in:String):Command = {
      in.split(" ").toList match {
        case "add"::name::Nil => Add(name)
        case "find"::partial::Nil => Find(partial)
        case _ => throw new IllegalArgumentException("unknow command")
      }
    }
  }
}

object Trie {
  case class Node(next:mutable.HashMap[Char, Node], var isEnd:Boolean, var subnodes:Int) {
  }
  def newEmptyNode(isEnd:Boolean) = new Node(mutable.HashMap.empty[Char, Trie.Node], isEnd, 0)
  def newTrie() = new Trie(newEmptyNode(false))
}

class Trie(topNode:Trie.Node) {
  def ensureNextNode(node:Trie.Node, c:Char):Trie.Node = {
    node.next.getOrElse(c, {
      val nn = Trie.newEmptyNode(false)
      node.next += ((c, nn))
      nn
    })
  }

  def add(a:String):Unit = {
    @tailrec
    def go(left:List[Char], node:Trie.Node): Unit = {
      node.subnodes += 1
      left match {
        case Nil =>
        case x :: Nil =>
          ensureNextNode(node, x).isEnd = true
        case x :: xs =>
          go(xs, ensureNextNode(node, x))
      }
    }
    go(a.toList, topNode)
  }

  def find(part:String) = {
    @tailrec
    def go(left:List[Char], node:Trie.Node):Int = left match {
      case x::xs  =>
        node.next.get(x) match {
          case None => 0
          case Some(nn) => go(xs, nn)
        }
      case Nil => if (node.isEnd) 1 + node.subnodes else node.subnodes
    }
    go(part.toList, topNode)
  }
}

class ContactsList {
  import ContactsList._
  var contacts = Trie.newTrie()
  var results = List.empty[Int]

  def executeCommand(cmd:Command):Unit = cmd match {
    case Add(name) => contacts.add(name)
    case Find(part) => results = (contacts.find(part)) :: results
  }
}

object ContactsApp  {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val contactsList = new ContactsList()
    for {_ <- 1 to n} {
      contactsList.executeCommand(ContactsList.Command.parseCommand(io.StdIn.readLine()))
    }
    Console.println(contactsList.results.reverse.mkString("\n"))
  }
}
