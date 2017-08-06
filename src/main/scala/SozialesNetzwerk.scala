// WS 2016 Aufgabe 5 SozialesNetzwerk
object SozialesNetzwerk {
  type Graph = List[List[Int]]

  def containsAll:(List[Int], List[Int]) => Boolean = (ps, ts) =>
    ts.foldLeft(true)((valid, el) => valid && ps.contains(el))

  def allFriends: (Graph, List[Int]) => List[Int] = (g, vs) =>
    (for(i <- List.range(0, g.length) if containsAll(i::g(i), vs)) yield i).filter(e => !vs.contains(e))
}
