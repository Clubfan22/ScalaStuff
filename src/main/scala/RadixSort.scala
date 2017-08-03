object RadixSort {
  def maxLength: List[String] => Int = strings => strings.map(_.length).max
  def charAt: String => Int => Char = string => pos => {
    if (string.length > pos){
      string.charAt(pos)
    } else {
      0.toChar
    }
  }
  def radixSort: List[String] => List[String] = inputs => {
    def helperRadix: List[String] => Int => List[String] = values => {
      case -1 => values
      case x => helperRadix(step(values)(x))(x-1)
    }
    def step: List[String] => Int => List[String] = values => pos => {
      for (c <- 0::('A' to 'Z').toList:::('a' to 'z').toList;
           value <- values if charAt(value)(pos) == c) yield value
    }
    helperRadix(inputs)(maxLength(inputs)-1)
  }
}
