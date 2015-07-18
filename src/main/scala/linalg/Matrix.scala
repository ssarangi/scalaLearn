package linalg

import linalg.LinAlgTypes.Row
import linalg.Vector._

/**
 * Created by sarangis on 7/13/15.
 * Adapted from this blog: https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
 * as well as http://etrain.github.io/2015/05/28/type-safe-linear-algebra-in-scala/
 */

/*
trait MatrixBinOps[T] {
  def +(that: Matrix[T])(implicit ev: Numeric[T]): Matrix[T]
  def -(that: Matrix[T])(implicit ev: Numeric[T]): Matrix[T]
  def *(that: Matrix[T])(implicit ev: Numeric[T]): Matrix[T]
}

trait Matrix[T] {
  // def zeros(length: Int)(implicit ev: Numeric[T]): Vector[T]
  // def toString(): String
  def data = List[Vector[T]]
}


object Matrix {
  def apply[T](args: Vector[T]*): Matrix[T] = new MatrixImpl[T](args.toList)

  private class MatrixImpl[@specialized(Double, Int, Float, Long)T](val _data: List[Vector[T]])
    extends Matrix[T]
    with MatrixBinOps[T] {
    def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl[T](_data.zip(that).map(elem => elem._1 + elem._2))//(_data.zip(that).map(elem => elem._1 + elem._2))
    def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl[T](_data.zip(that).map(elem => elem._1 - elem._2))
    def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl[T](_data.zip(that).map(elem => elem._1 * elem._2))

    // def fill(length: Int, value: T): Vector[T] = new VectorImpl[T](List.fill[T](length)(value))

    // def zeros(length: Int)(implicit ev: Numeric[T]): Vector[T] = fill(length, ev.zero)
    def toList(): List[Vector[T]] = _data

    // override def toString(): String = "Vector(" + _data.mkString(" , ") + ")"
  }

  implicit def MatrixToList[T](m: Matrix[T]): List[Vector[T]] = m.toList
}
*/

import Numeric.Implicits._

trait MatrixBinOps[T] {
  def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
  def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
  def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
}

trait Matrix[T] {
  def data: List[Row[T]]
  def zeros(m: Int)(implicit ev: Numeric[T]): Matrix[T]
  def zeros(m: Int, n: Int)(implicit ev: Numeric[T]): Matrix[T]
}

object Matrix {
  private class MatrixImpl[@specialized(Double, Int, Float, Long)T](val data: List[Row[T]]) extends Matrix[T] with MatrixBinOps[T] {

    def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this.data, that.data).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this.data, that.data).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this.data, that.data).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def zeros(m: Int)(implicit ev: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(m)(ev.zero)))
    def zeros(m: Int, n: Int)(implicit ev: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(n)(ev.zero)))
  }
}