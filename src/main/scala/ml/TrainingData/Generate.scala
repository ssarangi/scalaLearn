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

package ml.TrainingData

import linalg.LinAlgTypes._
import linalg.{Matrix, Vector}


/**
 * Created by sarangis on 7/12/15.
 */
object Generate {
  /** Generate line data for a bunch of points.
    * @param theta0: Constant for line equation
    * @param theta1: Constant which gets multiplied by x
    * @return DenseMatrix[Double]: returns this matrix containing y, x
   */
  def line_equation(theta0: Double, theta1: Double) : RegressionTrainingData = {
    val x0: VectorD = Vector[Double](List.fill[Double](200)(1.0))
    val x1: VectorD = Vector[Double]((-100.0 until 100.0 by 1.0).toList)
    val y: VectorD = theta0 * x0 + theta1 * x1

    val x = Matrix.empty[Double].add_col(x0).add_col(x1)
    new RegressionTrainingData(y, x)
  }

  /**
   * Generate a plane equation from 3 parameters
   * @param theta0
   * @param theta1
   * @param theta2
   * @return
   */
  def plane_equation(theta0: Double, theta1: Double, theta2: Double): RegressionTrainingData = {
    implicit val n = 2.0
    val x0 = Vector(List.fill[Double](200)(1.0))
    val x1 = Vector(List.tabulate[Double](200)(n => 2 * n + 1))
    val x2 = Vector(List.tabulate[Double](200)(n => 2 * n - 1))
    val y:  VectorD = theta0 * x0 + theta1 * x1 + theta2 * x2
    val x = Matrix.empty[Double].add_col(x0).add_col(x1).add_col(x2)
    new RegressionTrainingData(y, x)
  }
}