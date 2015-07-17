object session {
  import scala.math.BigInt
  val fibs: List[Int] = 0 :: 1 :: Nil
  val vibs =   fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  vibs
  vibs take 5 foreach println
}