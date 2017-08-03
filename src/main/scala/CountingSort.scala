/**
  * Created by Marco on 09.07.2017.
  */
object CountingSort {
  /*def incCount: (Int, List[Int]) => List[Int] = {
    case (0, Nil) => List(1)
    case (n, Nil) => 0::incCount(n-1, Nil)
    case (n, list) if (n >= list.length) => incCount(n, (0::(list.reverse)).reverse)
    case (n, list) => list.slice(0,n):::List(list(n)+1):::list.slice(n+1, list.length)
  } */
  def incCount: (Int, List[Int]) => List[Int] = {
    case (0, Nil) => List(1)
    case (n, Nil) => 0::incCount(n-1, Nil)
    case (0, h::list) => (h+1)::list
    case (n, h::list) => h::incCount(n-1, list)
  }

  def getCounts: List[Int] => List[Int] = {
    case Nil => Nil
    case h::list => incCount(h, getCounts(list))
  }

  def countingSort: List[Int] => List[Int] = {
    case Nil => Nil
    case list => countingSortHelper2(list, getCounts(list), 0)
  }

  private def countingSortHelper2: (List[Int], List[Int], Int) => List[Int] = {
    case (list, Nil, index) => Nil
    case (list, h::counts, index) => countingSortHelper(Nil, index, h) ::: countingSortHelper2(list, counts, index + 1)
  }

  private def countingSortHelper: (List[Int], Int, Int) => List[Int] = {
    case (list, value, 0) => list
    case (list, value, times) => countingSortHelper(value::list, value, times - 1)
  }
}
