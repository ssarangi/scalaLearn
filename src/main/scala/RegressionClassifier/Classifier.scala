package RegressionClassifier

/**
 * Created by sarangis on 7/11/15.
 */
trait Classifier {
  def fit_model() : Unit
  def predict() : Array[Double]
  def hypothesis[ThetaType, X_Type](theta: ThetaType, x: X_Type) : Double
  def cost_function() : Double
  def gradient_descent() : Unit
}
