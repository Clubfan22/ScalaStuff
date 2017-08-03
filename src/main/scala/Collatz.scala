/**
  * Created by Marco on 09.07.2017.
  */
object Collatz {
  def collatz: Int => Int = {
    case n if (n % 2 == 0) => n / 2
    case n => 3*n + 1
  }

  def collatzList: (Int, Int) => List[Int] = {
    case (n, 0) => List(n)
    case (n, h) => n::collatzList(collatz(n), h-1)
  }
  def collatzCycle: Int => Int = {
    case 4 => 0
    case n => collatzCycle(collatz(n)) + 1
  }
}
