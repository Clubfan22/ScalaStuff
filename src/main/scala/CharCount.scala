object CharCount {
  def absInc: (Map[Char, Int], Char) => Map[Char, Int] = (m, c) => {
    m + (c -> ((m getOrElse(c,0)) + 1))
  }

  def absCount: List[Char] => Map[Char, Int] = lc => {
    lc.foldLeft(Map[Char, Int]())((m, c) => absInc(m, c))
  }

  def streamCount: (Stream[Char], Map[Char, Int]) => Stream[(Char, Map[Char, Int])] = (sc, m) => {
    (sc.head, absInc(m, sc.head)) #:: streamCount(sc.tail, absInc(m, sc.head))
  }

  /*
  * So war es wahrscheinlich gedacht
  */
  def contAbsCount: Stream[Char] => Stream[(Char, Int)] = sc => {
    streamCount(sc, Map[Char, Int]()).map(e => (e._1, e._2(e._1)))
  }

  /*
  * Funktioniert, ist aber wahrscheinlich nicht so gewollt
  */
  def shittyContAbsCount: Stream[Char] => Stream[(Char, Int)] = sc => {
    def helper: Stream[Char] => Map[Char, Int] => Stream[(Char, Int)] = s => m => {
      val count = streamCount(s, Map[Char, Int]())
      (count.head._1, absInc(m, count.head._1)(count.head._1)) #:: helper(s.tail)(absInc(m, count.head._1))
    }
    helper(sc)(Map[Char, Int]())
  }
}
