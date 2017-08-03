object ListGenerators {
  def map: (Int => Int) => List[Int] => List[Int] =
    f => input => for (e <- input) yield f(e)

  def flatMap: (Int => List[Int]) => List[Int] => List[Int] =
    f => input => for (e <- input; ee <- f(e)) yield ee

  def pairSums: (Int, Int) => List[(Int, Int)] =
    (n, s) => for (e <- List.range(0, n+1) if e <= s && s-e <= n) yield (e, s-e)
}
