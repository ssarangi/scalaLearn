package RegressionClassifier

import CostFunction.CostFunctions._

/**
 * Created by sarangis on 7/11/15.
 */
abstract class RegressionClassifier extends Classifier {
  def fit_model() : Unit = {

  }

  def predict(): Array[Double] = {
    val result = Array(0.0, 0.0)
    result
  }

  def hypothesis[GenericType](theta: GenericType, x: GenericType) : Double = {
    theta match {
      case theta_a_d: Array[Double] => MultiVariableLinearRegressionCostFunc(theta_a_d, x)
      case theta_d: Double => SingleVariableLinearRegressionCostFunc(theta_d, x)
    }
  }
}
