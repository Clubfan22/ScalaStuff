/* https://www2.informatik.uni-erlangen.de/teaching/SS2014/PFP/uebungen/insecure/uebung11.pdf
 * SS 2014 Blatt 11 - Aufgabe 5: Buchstabenmix
 */

object CharMix {
  def mixIn: (Char, List[Char]) => List[List[Char]] = (c, lc) =>
    for (i <- List.range(0, lc.length + 1)) yield lc.take(i)::: c:: lc.drop(i)

  def mixInWords: (Char, List[List[Char]]) => List[List[Char]] = (c, css) =>
    (for (cs <- css) yield mixIn(c, cs)).flatten

  def mix: List[Char] => List[List[Char]] = {
    case Nil => Nil
    case c::cs => List(c)::mix(cs):::mixInWords(c, mix(cs))
  }
}
