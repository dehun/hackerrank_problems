object StringTask {
  val vowels = Set('A', 'O', 'Y', 'E', 'U', 'I')
  def isVowel(c:Char) = vowels.contains(c.toUpper)
  def main(args: Array[String]): Unit = {
    val s = io.StdIn.readLine()
    var rs = ""

    for {c <- s} {
      c match {
        case _ if isVowel(c) =>
        case _ => rs += "." + c.toLower
      }
    }
    Console.println(rs)
  }
}
