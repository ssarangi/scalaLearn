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

/**
 * Created by sarangis on 7/11/15.
 */

trait RegressionClassifier {

  def cost_func(hypothesis_func: (Vector[Double], Vector[Double]) => Double, training_data: Matrix[Double], theta: Vector[Double]): Double = {
    val y_training: Vector[Double] = training_data.col(0)
    val y_calculated: Vector[Double] = Vector((for (i <- 1 to training_data.cols) yield (hypothesis_func(theta, training_data.row(i).drop(1)))).toList)
    val num_training_data = training_data.rows.toDouble
    val subtracted_val: Vector[Double] = y_calculated - y_training
    val least_square_error = (subtracted_val * subtracted_val).sum.toDouble

    val cost: Double = 1 / (2 * num_training_data) * least_square_error
    cost
  }

  /*
  def fit_model[T <: LinearRegressionModel](hypothesis_func: (Vector[Double], Vector[Double]) => Double, training_data: Matrix[Double], theta: Vector[Double])(implicit m: Manifest[T]): T = {
    val type_class = m.asInstanceOf[T]
    type_class.create(Vector(1, 2, 3, 4))
  }
  */

  def fit_model(hypothesis_func: (Vector[Double], Vector[Double]) => Double, training_data: Matrix[Double], theta: Vector[Double]): LinearRegressionModel = {
    new LinearRegressionModel(Vector(1, 2, 3, 4))
  }
}
