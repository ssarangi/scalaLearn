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

package ml.Example

import linalg.Vector
import ml.RegressionClassifier.LinearRegressionClassifier
import ml.TrainingData.{Generate, RegressionTrainingData}

/**
 * Created by sarangis on 7/21/15.
 */
object LinearRegressionExample {
  def SingleVariableExample(): Unit = {
    // Main Entry function
    val training_data_theta0 = 1
    val training_data_theta1 = 2

    val training_data: RegressionTrainingData = Generate.line_equation(training_data_theta0, training_data_theta1)
    println(s"Running Linear Regression with initial parameters: $training_data_theta0, $training_data_theta1")
    println("-" * 100)

    val initial_theta0_guess = 2
    val initial_theta1_guess = 10

    println(s"Starting with initial guess: $initial_theta0_guess, $initial_theta1_guess")
    println("-" * 100)

    val learning_rate = 0.0001
    val convergence_tol = 0.000001
    val max_iterations = 100000

    val classifier = new LinearRegressionClassifier(learning_rate, convergence_tol, max_iterations)
    classifier.train(training_data, Vector[Double](initial_theta0_guess, initial_theta1_guess))
  }

  def MultiVariableExample(): Unit = {
    // Main Entry function
    val training_data_theta0 = 1
    val training_data_theta1 = 2
    val training_data_theta2 = 3

    val training_data: RegressionTrainingData = Generate.plane_equation(training_data_theta0, training_data_theta1, training_data_theta2)
    println(s"Running Linear Regression with initial parameters: $training_data_theta0, $training_data_theta1, $training_data_theta2")
    println("-" * 100)

    val initial_theta0_guess = 2
    val initial_theta1_guess = 3
    val initial_theta2_guess = 4

    println(s"Starting with initial guess: $initial_theta0_guess, $initial_theta1_guess, $initial_theta2_guess")
    println("-" * 100)

    val learning_rate = 0.000001
    val convergence_tol = 0.000001
    val max_iterations = 100000

    val classifier = new LinearRegressionClassifier(learning_rate, convergence_tol, max_iterations)
    classifier.train(training_data, Vector[Double](initial_theta0_guess, initial_theta1_guess, initial_theta2_guess))
  }

}
