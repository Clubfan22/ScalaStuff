import LabyrinthPredef._

object Labyrinth {
  def getNeighbors: Rooms => Room => List[Room] = rooms => room =>
    for (dir <- List.range(0,4) if !room.hasWall(dir) && rooms.contains(room.nextCoords(dir))) yield rooms(room.nextCoords(dir))

  def findSolution: Rooms => (Room, Room) => List[Room] = rooms => (start, end) => {
    helper(rooms)(start, end)(Nil)
  }

  private def helper: Rooms => (Room, Room) => List[Room] => List[Room] = rooms => (start, end) => path => {
    if (start == end){
      end::path
    } else if (path.contains(start)) {
        Nil
    } else {
      (for (nb <- getNeighbors(rooms)(start)) yield helper(rooms)(nb, end)(start::path)).flatten
    }
  }
}


