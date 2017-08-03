object Par {
  def pairSumsPar: (Int, Int) => List[(Int, Int)] = (n, s) => {
    (for (i <- List.range(0, n+1).par if (i <= s && s-i <= n)) yield (i, s-i)).toList
  }
}
