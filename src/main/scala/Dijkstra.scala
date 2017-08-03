object Dijkstra {
  def reachable: List[List[(Int, Int)]] => List[(Int, Int)] => List[(Int, Int)] = al => vs =>
    for ((v, vk) <- vs; (p, pk) <- al(v) if !vs.map(_._1).contains(p)) yield (p, vk + pk)

  def shortest: (List[(Int, Int)] => List[(Int, Int)]) => Int => List[(Int, Int)] = f => st => {
    (st,0)::shortestHelper(f)(st)(List((st, 0)))
  }

  def shortestHelper: (List[(Int, Int)] => List[(Int, Int)]) => Int => List[(Int, Int)] => List[(Int,Int)] = f => node => vs => {
    if (f(vs) == Nil){
      Nil
    } else {
      val min = f(vs).minBy(_._2)
      min::shortestHelper(f)(min._1)(min::vs)
    }
  }
}
