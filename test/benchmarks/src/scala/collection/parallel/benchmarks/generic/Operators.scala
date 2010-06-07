package scala.collection.parallel.benchmarks.generic






trait Operators[T] {
  
  def reducer: (T, T) => T
  def mediumreducer: (T, T) => T
  def filterer: T => Boolean
  def mapper: T => T
  def heavymapper: T => T
  def taker: T => Boolean
  
}



trait IntOperators extends Operators[Int] {
  
  val reducer: (Int, Int) => Int = _ + _
  val mediumreducer: (Int, Int) => Int = (a: Int, b: Int) => {
    val result = if (b == 0) a else {
      mediumreducer.apply(b, a - b * (a / b))
    }
    result + 1000
  }
  val filterer: Int => Boolean = _ % 2 == 0
  val mapper: Int => Int = _ * 2
  val heavymapper: Int => Int = (n: Int) => {
    var i = -10
    var sum = 0
    while (i < 0) {
      sum += -i
      i += 1
    }
    n + sum
  }
  val taker: Int => Boolean = _ < 10000
  
}








