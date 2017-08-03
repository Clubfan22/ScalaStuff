object Lazy {
  def natural : Stream[Long] = {
    def helper: Long => Stream[Long] = a => a #:: helper(a+1)
    helper(1)
  }
  def limit: Stream[Long] => Long => List[Long] = {
    case x #:: xs => i => if (x <= i) x :: limit(xs)(i) else Nil
  }
  def interval: Stream[Long] => (Long, Long) => List[Long] = {
    case x #:: xs => (k, l) => if (x <= l){
      if (x >= k){
        x :: interval(xs)(k,l)
      } else {
        interval(xs)(k,l)
      }
    } else {
      Nil
    }
  }

  def factorial: Int => Long = i => {
    def facHelp: (Long, Int) => Stream[Long] = (a, b) => a#::facHelp(a * (b+1), b+1)
    facHelp(1,0)(i)
  }

  def euler: Int => Double = n => {
    def eulHelp: Int => Stream[Double] = i => {
      (1.0 / factorial(i)) #:: eulHelp(i+1)
    }
    eulHelp(0).take(n).sum
  }
}
