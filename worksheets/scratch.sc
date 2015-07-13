object session {
  def generateLineData(m: Double, c: Double) : Int = {
    var x = (-200 to 200 by 1).toVector
    val y = ((-200 to 200 by 1) map { _ * m + c })
    println(x)
    println(y)
    1
  }

  generateLineData(1, 1)

  val addOne = (x: Int) => { x * 1.2 } : Double

  val r = addOne(4)

  def adder(m: Int, n: Int) = m + n

  val add2 = adder(2, _:Int)
  add2(3)

  def multiply(m: Int)(n: Int) = m * n

  multiply(2)(3)

  val timesTwo = multiply(2)_

  timesTwo(3)

  val curriedAdd = (adder _).curried

  val addTwo = curriedAdd(2)
  addTwo(2)

  def add(x: Int, y: Int) : Int = x + y
  add(1, 3)
  add(1, 5)

  def add(x: Int) = (y: Int) => x + y
  add(1)(2)
  add(11)(4)


  def new_add(x:Int, y:Int) = x + y
  val add_curried = (new_add _).curried

  add_curried(3)

  def sum(args: Int*) = {
    var result = 0
    for (arg <- args) result += arg
    result
  }

  sum(1, 2, 3, 4, 5)
  val s = sum(1 to 5: _*)

  List(1, 2, 4) foreach { _ => println("Hi") }

  1 to 10 foldleft(_ + _)

}