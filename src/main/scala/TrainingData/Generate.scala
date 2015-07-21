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
    val x: Vector[Double] = Vector((-100.0 to 100.0 by 50.0).toList)
    val y_tmp: Vector[Double] = theta0 + theta1 * x

    Matrix.empty[Double].add_col(y_tmp).add_col(x)
  }

  def plane_equation(theta0: Double, theta1: Double, theta2: Double): Matrix[Double] = {
    implicit val n = 2.0
    val x0: Vector[Double] = Vector(List.tabulate[Double](200)(n => 2 * n + 1))
    val x1: Vector[Double] = Vector(List.tabulate[Double](200)(n => 2 * n - 1))
    val y_tmp: Vector[Double] = theta0 + theta1 * x0 + theta2 * x1
    Matrix.empty[Double].add_col(y_tmp).add_col(x0).add_col(x1)
  }
}