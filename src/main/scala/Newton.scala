import NewtonPredef._
object Newton {
  def newton : ((Double => Double), (Double => Double)) => Double => Stream[Double] = (f, fderiv) => x0 => {
    x0 #:: newton(f, fderiv)(x0 - (f(x0)/fderiv(x0)))
  }
  
  def getZero : Int => Stream[Double] => Option[Double] = m => s => {
    if (m == 1){
      None
    } else {
      if (equal(s(0), s(1))){
        Some(s(1))
      } else {
        getZero(m-1)(s.drop(1))
      }
    }   
  }

  def main (args :Array[String]): Unit ={
    val res = newton(f5, deriv(f5))(1)
    println(res.take(10).toList)
    println(getZero(70)(res))
  }
}