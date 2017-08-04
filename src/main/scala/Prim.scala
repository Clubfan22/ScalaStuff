object Prim {
  def getPossibleEdges: List[List[(Int, Int)]] => List[Int] => List[(Int, Int, Int)] = al => rs => {
    for (s <- rs; (d, c) <- al(s) if !rs.contains(d)) yield (s, d,c)
  }
  def mst: (List[Int] => List[(Int, Int, Int)]) => Int => List[(Int, Int)] = f => st => {
    def helper: List[Int] => List[(Int,Int)] = rs => {
      val left = f(rs)
      if (left == Nil){
        Nil
      } else {
        val min = left.minBy(_._3)
        (min._1, min._2)::helper(min._2::rs)
      }
    }
    helper(List(st))
  }
}
