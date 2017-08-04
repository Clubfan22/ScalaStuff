object ListOperations {
  def flatten: List[(Char, Char)] => List[Char] = {
    case Nil => Nil
    case x::xs => x._1 :: x._2 :: flatten(xs)
  }

  def countPar: List[Char] => List[(Char, Int)] = cs => {
    for (c <- cs) yield (c, cs.par.count(_ == c))
  }

  def distinctWithSet[A]: List[A] => List[A] = xs => {
    xs.foldLeft(Set[A]())((s, e) => s + e).toList
  }

  def distinct[A]: List[A] => List[A] = xs => {
    xs.foldLeft(List[A]())((l, e) => if (l.contains(e)) l else e::l)
  }
}
