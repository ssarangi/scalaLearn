import TrainingData.Generate
// import RegressionClassifier.LinearRegressionClassifier
import breeze.linalg.DenseMatrix
import linalg._

/**
 * Created by sarangis on 7/12/15.
 */
object scalaLearnApp extends App {
  def main(): Unit = {

    // Try the vector
    val b = Vector(1, 2, 3, 4)

    // Main Entry function
    val training_data_theta0 = 1
    val training_data_theta1 = 1

    val training_data: DenseMatrix[Double] = Generate.line_data(training_data_theta0, training_data_theta1)
    println(s"Running Linear Regression with initial parameters: $training_data_theta0, $training_data_theta1")

    println("Training Data:")
    println("-" * 100)
    // println(training_data.deep.mkString("\n"))

    val initial_theta0_guess = 4
    val initial_theta1_guess = 5

    // val linear_regression_classifier : LinearRegressionClassifier = new LinearRegressionClassifier(training_data)
  }

  main()
}
