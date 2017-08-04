import GameOfLifePredef._
object GameOfLife {
  def aliveNeighbors: (Cell => List[Cell]) => Cell => Int = nb => c => nb(c).count(_.alive)

  def tick: Field => Field = f => {
    f.map(c => Cell(c.x, c.y, aliveNeighbors(neighbors(f))(c) match {
      case 3 => true
      case x if x < 2 || x > 3 => false
      case _ => c.alive
    }))
  }

  def game: Field => Stream[Field] = f => {
    f #:: game(tick(f))
  }
}
