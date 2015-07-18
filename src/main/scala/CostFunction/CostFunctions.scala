package CostFunction

/*
object CostFunctions {
  def SingleVariableLinearRegressionCostFunc(theta0: Double, theta1: Double, x: Double): Double = {
    // This is a line equation
    val hypothesis = theta0 + theta1 * x
    hypothesis
  }

  def multiply_array(x: Array[Double], y: Array[Double], agg: Array[Double], i: Int): Array[Double] = {
    if (i == x.length) agg
    else {
      agg(i) = x(i) * y(i)
      multiply_array(x, y, agg, i)
    }
  }

  def MultiVariableLinearRegressionCostFunc(theta: Array[Double], x: Array[Double]) : Double = {
    // This is a plane equation
    val agg = multiply_array(theta, x, new Array(x.length), 0)
    val hypothesis = agg.transpose.map(_.sum)
    hypothesis.sum
  }
}
*/