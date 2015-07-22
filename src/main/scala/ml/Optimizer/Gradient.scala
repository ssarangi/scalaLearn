/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Satyajit Sarangi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

package ml.Optimizer

import linalg.LinAlgTypes._
import ml.TrainingData.{BaseTrainingData, RegressionTrainingData}
import ml.HypothesisFunctions.HypothesisFunctions._
import Numeric.Implicits._

/**
 * Created by sarangis on 7/21/15.
 */
trait Gradient[TrainingDataType <: BaseTrainingData] {

  /**
   * Compute the gradient of the function
   * @param t: Training data
   * @param theta: Theta (Coefficients) of the function
   */
  def compute(t: TrainingDataType, theta: VectorD): Double

  /**
   * Compute the derivative of the Gradient function
   * @param t: training data
   * @param theta: Theta parameters of the equation (Coefficients)
   * @param theta_idx: Which Theta index (for ex theta0, theta1, theta2 etc) is being calculated
   * @return Double: Returns the derivative value for the gradient descent function. This needs to be multiplied by
   *         learning rate to update the theta
   */
  def derivative(t: TrainingDataType, theta: VectorD, theta_idx: Int): Double
}

class LeastSquaresGradient(val hypothesis: RegressionHypothesisFunctorTy) extends Gradient[RegressionTrainingData] {

  override def compute(training_data: RegressionTrainingData, theta: VectorD): Double = {
    val y_training:   VectorD       = training_data.y
    val y_calculated: VectorD       = (for (i <- 0 until training_data.length) yield (hypothesis(theta, training_data.x.row(i)))).toList

    val subtracted_val: VectorD     = y_calculated - y_training

    val least_square_error: Double  = (subtracted_val * subtracted_val).sum.toDouble

    val cost: Double = 1 / (2 * training_data.length.toDouble) * least_square_error
    cost
  }

  def derivative(training_data: RegressionTrainingData, theta: VectorD, theta_idx: Int): Double = {
    val y_training:   VectorD       = training_data.y
    val y_calculated: VectorD       = (for (i <- 0 until training_data.length) yield (hypothesis(theta, training_data.x.row(i)))).toList

    val subtracted_val: VectorD = y_calculated - y_training

    val derivative = (subtracted_val * training_data.x.col(theta_idx)).sum

    (1 / training_data.length.toDouble) * derivative
  }
}
