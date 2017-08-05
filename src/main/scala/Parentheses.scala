//https://www2.informatik.uni-erlangen.de/teaching/SS2015/PFP/uebungen/secure/uebung11_slides.pdf 11.5 Klammern
object Parentheses {
  def parenthesize: String => List[String] = {
    case null | "" => Nil
    case s => (s::"("+s+")"::(
      for(i <- List.range(0, s.length); a <- parenthesize(s.take(i)); b <- parenthesize(s.drop(i)))
        yield List(a+b, "("+a+b+")")
      ).flatten).distinct
  }
}
