object Huffman {
  abstract class Node(val freq: Int)
  case class Leaf(f: Int, c: Char) extends Node(f)
  case class Inner(f: Int, zero: Node, one: Node) extends Node(f)

  def freq: List[Char] => List[Leaf] = {
    case Nil => Nil
    case c::cs => Leaf(1 + cs.count(_ == c), c)::freq(cs.filterNot(_ == c))
  }

  def merge: List[Node] => Node = l => {
    def mergeHelper: List[Node] => Node = {
      case Nil => ???
      case x::Nil => x
      case x::y::cs => merge((Inner(x.freq + y.freq, x, y)::cs).sortBy(_.freq))
    }
    mergeHelper(l.sortBy(_.freq))
  }

  type CharEnc = (Char, List[Int])

  def getEnc: Node => List[CharEnc] = {
    case Leaf(f,c) => List((c, Nil))
    case Inner(f, zero, one) =>
      (for (e <- getEnc(zero))
        yield(e._1, 0::e._2)
        ) :::
      (for (e <- getEnc(one))
        yield(e._1, 1::e._2)
        )
  }

  def encode: List[CharEnc] => Stream[Char] => Stream[Int] = enc =>
    cs => cs.map(c => enc.filter(_._1 == c)(0)._2).flatten
}
