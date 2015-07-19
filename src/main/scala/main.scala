import TrainingData.Generate
// import RegressionClassifier.LinearRegressionClassifier
import breeze.linalg.DenseMatrix
import linalg.{Vector, Matrix}

/**
 * Created by sarangis on 7/12/15.
 */
object scalaLearnApp extends App {
  def main(): Unit = {

    // Try the vector
    val b = Vector(1, 2, 3, 4)
    val b1 = Vector(4, 5, 6, 7)

    val hh = b1 + b

    val m = Matrix(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

    val m1 = m.add_row(List(Vector(10, 11, 12).toList))
    val m2 = m.add_col(List(List(10, 11, 12)))

    // println(m)
    // println(m.transpose)

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
