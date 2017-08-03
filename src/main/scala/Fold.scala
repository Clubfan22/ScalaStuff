object Fold {
  def foldRight: String => ((Int, String) => String) => List[Int] => String = ys => f => {
    case x::Nil => f(x, ys)
    case x::xs => f(x, foldRight(ys)(f)(xs))
  }

  def foldLeft: String => ((String, Int) => String) => List[Int] => String = ys => f => {
    case x::Nil => f(ys, x)
    case x::xs => foldLeft(f(ys, x))(f)(xs)
  }
}
