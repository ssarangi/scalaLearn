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

package RegressionClassifier

import Classifier.{Model, Classifier}
import linalg.{Vector, Matrix}
import linalg.RichListObject._

/**
 * Created by sarangis on 7/11/15.
 */

trait RegressionClassifier {

  def cost_func(hypothesis_func: (List[Double], List[Double]) => Double, training_data: Matrix[Double], theta: List[Double]): Double = {
    val num_training_data  = training_data.rows.toDouble

    val y_training         = training_data.col(0)
    val y_calculated       = (for (i <- 0 until training_data.rows) yield (hypothesis_func(theta, training_data.row(i).tail))).toList

    val subtracted_val     = y_calculated - y_training

    val least_square_error = (subtracted_val * subtracted_val).sum.toDouble

    val cost: Double = 1 / (2 * num_training_data) * least_square_error
    cost
  }

  /**
   * @param hypothesis_func:
   * @param training_data:
   * @param theta:
   * @param learning_rate: Learning rate for the gradient descent
   * @param j: Compute for Theta_j'th value for this where j = number of parameters in the linear regression
   */
  def derivative_cost_func(hypothesis_func: (List[Double], List[Double]) => Double, training_data: Matrix[Double], theta: List[Double], learning_rate: Double, j: Int): Double = {
    val num_training_data  = training_data.rows.toDouble

    val y_training         = training_data.col(0)
    val y_calculated       = (for (i <- 0 until training_data.rows) yield (hypothesis_func(theta, training_data.row(i).tail))).toList

    val subtracted_val     = y_calculated - y_training

    val x_j = for (k <- 0 until training_data.rows) yield(training_data.col(j + 1))

    val derivative = (subtracted_val * training_data.col(j + 1)).sum

    (1 / num_training_data) * learning_rate * derivative
  }

  /*
  def fit_model[T <: LinearRegressionModel](hypothesis_func: (Vector[Double], Vector[Double]) => Double, training_data: Matrix[Double], theta: Vector[Double])(implicit m: Manifest[T]): T = {
    val type_class = m.asInstanceOf[T]
    type_class.create(Vector(1, 2, 3, 4))
  }
  */

  import annotation.tailrec

  @tailrec final def stochastic_gradient_descent(hypothesis_func: (List[Double], List[Double]) => Double, training_data: Matrix[Double], theta: List[Double], learning_rate: Double, threshold: Double, current_iteration: Int, max_iterations: Int): List[Double] = {

    val old_cost = cost_func(hypothesis_func, training_data, theta)
    val num_variables = training_data.cols - 1
    val tmp_theta = Vector((for (j <- 0 until num_variables) yield(derivative_cost_func(hypothesis_func, training_data, theta, learning_rate, j))).toList)
    val new_theta = theta - tmp_theta

    val new_cost = cost_func(hypothesis_func, training_data, new_theta)

    println(s"Iteration: $current_iteration --> theta: $new_theta --> cost: $new_cost")

    if (new_cost < threshold || current_iteration > max_iterations)
      new_theta
    else
      stochastic_gradient_descent(hypothesis_func, training_data, new_theta, (if (new_cost > old_cost) learning_rate / 10 else learning_rate), threshold, current_iteration + 1, max_iterations)
  }

  def fit_model(hypothesis_func: (List[Double], List[Double]) => Double, training_data: Matrix[Double], theta: List[Double], learning_rate: Double, threshold: Double, max_iterations: Int): LinearRegressionModel = {
    val converged_theta = stochastic_gradient_descent(hypothesis_func, training_data, theta, learning_rate, threshold, 0, max_iterations)
    new LinearRegressionModel(converged_theta)
  }
}
