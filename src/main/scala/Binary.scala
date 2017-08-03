
object Binary {

  /*
  Absolut bescheuerte und uebertriebene Loesung, die ich am Anfang hatte
  def parseLong : List[Char] => Long = {

      case Nil => 0
      case bs => parseLongHelper(generateListWithTwoPot(bs.reverse) (0))(0)
  }

  def generateTwoPot: Long => Long = {
    case 0 => 1
    case n => 2 * generateTwoPot(n - 1)
  }

  def generateListWithTwoPot : List[Char] => Long => List[(Char, Long)] = {
    case Nil => e => Nil
    case h::bs => e => (h, generateTwoPot(e))::generateListWithTwoPot(bs)(e+1)
  }

  def parseLongHelper:List[(Char, Long)] => Long => Long = {
    case Nil => value => value
    case h::bs => value => parseLongHelper(bs) (value +  toNumber(h._1) * h._2)
  }

  def toNumber: Char => Long = {
    case '0' => 0
    case '1' => 1
  }
  */
  def parseLong: List[Char] => Long = {
    case Nil => 0
    case bs => parseLongHelper(bs.reverse, 0, 1)
  }
  private def parseLongHelper: (List[Char], Long, Long) => Long = {
    case (Nil, value, exp) => value
    case (h::bs, value, exp) => parseLongHelper(bs, value + exp * (if (h == '0') 0 else 1), exp * 2)
  }
}