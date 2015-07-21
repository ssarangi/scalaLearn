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

import linalg.{Vector, Matrix}
import CostFunction.CostFunctions.linear_regression_hypothesis_func

/**
 * Created by sarangis on 7/11/15.
 */

object LinearRegressionClassifier
  extends RegressionClassifier {

  def fit_model(training_data: Matrix[Double], initial_theta: Vector[Double], learning_rate: Double, threshold: Double, max_iterations: Int): LinearRegressionModel = {
    val augmented_training_data = training_data.insert_col(1, Vector(List.fill[Double](training_data.rows)(1.0)))
    super.fit_model(linear_regression_hypothesis_func, augmented_training_data, initial_theta, learning_rate, threshold, max_iterations)
  }

}
