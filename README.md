# Trie (immutable trie implementation)

```scala
sealed trait Trie {

  /**
    *
    * @param word String to add to the trie
    * @return Returns the immutable new trie got created after the string addition
    */
  def add(word: String): Trie = {

    def navigate(rest: List[Char], currentNode: Trie): Trie = (rest, currentNode) match {
      case (Nil, node: TrieNode) => TrieNode(node.next, wordEnding = true)
      case (Nil, Empty) => Empty
      case (x :: xs, Empty) => TrieNode(Map(x -> navigate(xs, Empty)))
      case (x :: xs, TrieNode(nextMap, _)) if nextMap contains x =>
        TrieNode(nextMap + (x -> navigate(xs, nextMap(x))))
      case (x :: xs, TrieNode(nextMap, _)) => TrieNode(nextMap + (x -> navigate(xs, Empty)))
    }

    navigate(word.toLowerCase.toList, this)
  }

  /**
    * @param word String to search in the trie
    * @return returns true if the word is found in the trie
    */
  def find(word: String): Boolean = {

    @scala.annotation.tailrec
    def navigate(rest: List[Char], currentNode: Trie): Boolean = (rest, currentNode) match {
      case (Nil, Empty) => true
      case (Nil, node: TrieNode) if node.wordEnding => true
      case (_, Empty) => false
      case (c :: cs, TrieNode(nextMap, _)) if nextMap contains c => navigate(cs, nextMap(c))
      case (_, _) => false
    }

    navigate(word.toLowerCase.toList, this)
  }


  /**
    *
    * @return all words represented by the trie
    */
  def words: List[String] = {

    def navigate(currentNode: Trie): List[String] = currentNode match {
      case TrieNode(next, _) => next.map {
        case (k, Empty) => List(k.toString)
        case (k, deepNode: TrieNode) if deepNode.wordEnding =>
          ("" :: navigate(deepNode)).map(str => (k :: str.toList).mkString)
        case (k, deepNode) => navigate(deepNode).map(str => (k :: str.toList).mkString)
      }.foldLeft(List.empty[String])(_ ++ _)
      case Empty => List.empty[String]
    }

    navigate(this)
  }

}

case class TrieNode(next: Map[Char, Trie], wordEnding: Boolean = false) extends Trie
case object Empty extends Trie
```
