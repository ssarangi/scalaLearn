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
