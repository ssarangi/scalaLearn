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
import linalg.Vector
import ml.HypothesisFunctions.HypothesisFunctions.RegressionHypothesisFunctorTy
import ml.Optimizer.Updater._
import ml.TrainingData.RegressionTrainingData

import scala.annotation.tailrec

/**
 * Created by sarangis on 7/21/15.
 */
class GradientDescent(val hypothesis_func: RegressionHypothesisFunctorTy) {
  private var gradient      : Gradient[RegressionTrainingData] = new LeastSquaresGradient(hypothesis_func)
  private var updater       : UpdaterFunctorTy = simpleUpdaterFunctor
  private var learning_rate : Double = 0.0001
  private var convergeTol   : Double = 0.01
  private var max_iterations: Double = 50000

  def setGradientFunc(g: Gradient[RegressionTrainingData]): this.type = {
    this.gradient = g
    this
  }

  def setUpdaterFunc(up: UpdaterFunctorTy): this.type = {
    this.updater = up
    this
  }

  def setLearningRate(lr: Double): this.type = {
    this.learning_rate = lr
    this
  }

  def setMaxIterations(max_iter: Double): this.type = {
    this.max_iterations = max_iter
    this
  }

  def setConverganceTol(tol: Double): this.type = {
    this.convergeTol = tol
    this
  }

  def run(training_data: RegressionTrainingData, theta: VectorD): VectorD = {
    @tailrec def stochasticGradientDescent(current_theta: VectorD, learning_rate: Double, current_iteration: Int): Vector[Double] = {
      val old_cost = gradient.compute(training_data, current_theta)
      val nv = training_data.variables
      val tmp_theta = Vector((for (j <- 0 until training_data.variables)
        yield(gradient.derivative(training_data, current_theta, j))).toList)

      val theta_with_lr = tmp_theta * this.learning_rate
      val new_theta = updater(current_theta, theta_with_lr)

      val (is_converged, new_cost) = isConverged(new_theta)
      println(s"Iteration: $current_iteration --> theta: $new_theta --> cost: $new_cost")

      val new_learning_rate = if (new_cost > old_cost) learning_rate / 10 else learning_rate

      if (is_converged || current_iteration > this.max_iterations)
        new_theta
      else
        stochasticGradientDescent(new_theta, new_learning_rate, current_iteration + 1)
    }

    def isConverged(theta: VectorD): (Boolean, Double) = {
      val cost: Double = gradient.compute(training_data, theta)
      if (cost < convergeTol) (true, cost) else (false, cost)
    }

    stochasticGradientDescent(theta, this.learning_rate, 0)
  }
}
