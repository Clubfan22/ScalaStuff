object Flip {
  def flipDrehen: Map[Int, String] => Map[String, Set[Int]] = src =>
    src.foldLeft(Map[String, Set[Int]]())(elementDrehen)

  def elementDrehen: (Map[String, Set[Int]], (Int, String)) => Map[String, Set[Int]] = {
    case (m, (key, value)) =>
      if (m.isDefinedAt(value)) {
        m updated(value, m(value) + key)
      } else {
        m + ((value, Set[Int](key)))
      }
  }
  def flipOld: Map[Int, String] => Map[String, Set[Int]] = src =>
    src.foldLeft(Map[String, Set[Int]]())((m, element) =>
      if (m.isDefinedAt(element._2)) {
        m updated (element._2, m(element._2) + element._1)
      } else {
        m + ((element._2, Set[Int](element._1)))
      })

  def flipGen[A,B]: Map[A, B] => Map[B, Set[A]] = src =>
    src.foldLeft(Map[B, Set[A]]())((m, element) =>
      m + (element._2 -> ((m getOrElse(element._2, Set[A]())) + element._1))
    )

  def backFlipGen [A,B]: Map[A, Set[B]] => Map[B, A] = src =>
    src.foldLeft(Map[B, A]()) ((m, el) =>  el._2.foldLeft(m)((m2, el2) => m2 + (el2 -> el._1)))


  def flip: Map[Int, String] => Map[String, Set[Int]] = src =>
    src.foldLeft(Map[String, Set[Int]]())((m, element) =>
      m + (element._2 -> ((m getOrElse(element._2, Set[Int]())) + element._1))
    )

  def backFlip: Map[String, Set[Int]] => Map[Int, String] = src =>
    src.foldLeft(Map[Int, String]()) ((m, el) =>  el._2.foldLeft(m)((m2, el2) => m2 + (el2 -> el._1)))

  def backFlipJulia: Map[String, Set[Int]] => Map[Int, String] = x =>
    x.foldLeft(Map[Int, String]()) ((n, element) =>
      element._2.foldLeft(n)((neu,value) => neu + ((value,element._1))))
}
