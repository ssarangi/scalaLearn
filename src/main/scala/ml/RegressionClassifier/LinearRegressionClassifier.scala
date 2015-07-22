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

package ml.RegressionClassifier

import linalg.LinAlgTypes.{MatrixD, VectorD}
import linalg.{Vector, Matrix}
import ml.Classifier.Classifier
import ml.HypothesisFunctions.HypothesisFunctions._
import ml.Optimizer.{Updater, LeastSquaresGradient, GradientDescent}
import ml.TrainingData.RegressionTrainingData
import ml.Optimizer.Updater._

/**
 * Created by sarangis on 7/11/15.
 */

class LinearRegressionClassifier(private val learning_rate: Double,
                                 private val convergence_tol: Double,
                                 private val max_iterations: Int)
  extends Classifier[LinearRegressionModel, RegressionTrainingData] {

  private val gradientDescent = new GradientDescent(regressionHypothesisFunctor)
                                .setGradientFunc(new LeastSquaresGradient(regressionHypothesisFunctor))
                                .setUpdaterFunc(simpleUpdaterFunctor)
                                .setConverganceTol(convergence_tol)
                                .setLearningRate(learning_rate)
                                .setMaxIterations(max_iterations)

  def train(training_data: RegressionTrainingData, initial_theta: VectorD): LinearRegressionModel = {
    // Check the training data. If the number of variables is less than the number of theta parameters then that means
    // the data wasn't augmented with the 1's for Theta0
    require(initial_theta.length == training_data.variables,
            "Number of variables should be equal to the number of coefficient's passed." +
            " Did you forget to initialize x0 to 1's")

    new LinearRegressionModel(gradientDescent.run(training_data, initial_theta))
  }
}
