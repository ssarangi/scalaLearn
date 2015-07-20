package TrainingData

import linalg.{Vector, Matrix}

import scala.collection.immutable.NumericRange

/**
 * Created by sarangis on 7/12/15.
 */
object Generate {
  /** Generate line data for a bunch of points.
    * @param theta0: Constant for line equation
    * @param theta1: Constant which gets multiplied by x
    * @return DenseMatrix[Double]: returns this matrix containing y, x
   */
  def line_data(theta0: Double, theta1: Double) : Matrix[Double] = {
    val x: Vector[Double] = Vector((-100.0 to 100.0 by 1.0).toList)
    val y_tmp: Vector[Double] = theta0 + theta1 * x

    Matrix.empty[Double].add_col(y_tmp).add_col(x)
  }
}