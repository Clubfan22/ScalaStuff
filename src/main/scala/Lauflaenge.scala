object Lauflaenge {
  def decodeTuple: ((Char, Int)) => Stream[Char] = {
    case (c,k) if k <= 0 => Stream[Char]()
    case (c, k) => c #::decodeTuple((c, k-1))
  }
  def decode: Stream[(Char, Int)] => Stream[Char] = {
    case Stream.Empty => Stream.Empty
    case s => decodeTuple(s.head)#:::decode(s.tail)
  }
}
