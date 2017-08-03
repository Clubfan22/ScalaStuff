import MasterMindSkeleton.Color._

object MasterMind {
  def tupelize: List[Color] => List[(Color, Int)] = l => {
    def helper: List[Color] => Int => List[(Color, Int)] = in => i => in match {
      case Nil => Nil
      case l::ls => (l, i)::helper(ls)(i+1)
    }
    helper(l)(1)
  }

  def deTupelize: List[(Color, Int)] => List[Color] = l => l.map(_._1)

  def masterMind: (List[Color], List[Color]) => (Int, Int) = (solution, guess) => {
    val samePos = tupelize(guess).intersect(tupelize(solution))
    val colContained = guess.intersect(deTupelize(tupelize(solution).diff(samePos)))
    (samePos.length, colContained.length)
  }
}
