object Curry {
  def curry: ((Int, Int) => Int) => (Int => Int => Int) =
    f => a => b => f(a, b)

  def uncurry: (Int => Int => Int) => ((Int, Int) => Int) =
    f => (a,b) => f(a)(b)
}
