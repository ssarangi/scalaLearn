package TrainingData

import breeze.linalg._
import breeze.numerics.Scaling

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
  def line_data(theta0: Double, theta1: Double) : DenseMatrix[Double] = {
    val x: Array[Double] = (-100.0 to 100.0 by 1.0).toArray
    val y_tmp: Array[Double] = Array.fill[Double](x.length)(1.0).map(_ * theta0)
    val m = x.map(_ * theta1)
    val y = Array(y_tmp, x).transpose.map(_.sum)
    DenseMatrix(y, x).t // Take the transpose of this matrix so we will have the form y, x0, x1, x2, .... , xn
  }
}