package TrainingData

/**
 * Created by sarangis on 7/11/15.
 */
trait TrainingData {
  type T
  val training_data: Array[T]
}
