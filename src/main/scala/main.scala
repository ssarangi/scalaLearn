import TrainingData.Generate
// import RegressionClassifier.LinearRegressionClassifier
import breeze.linalg.DenseMatrix
import linalg.{Vector, Matrix}

/**
 * Created by sarangis on 7/12/15.
 */
object scalaLearnApp extends App {
  def main(): Unit = {

    val test = Vector(1, 2, 3, 4)
    val l = test.length
    // Main Entry function
    val training_data_theta0 = 1
    val training_data_theta1 = 2

    val training_data: Matrix[Double] = Generate.line_data(training_data_theta0, training_data_theta1)
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
