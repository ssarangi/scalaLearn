package linalg

import scala.specialized

/**
 * Created by sarangis on 7/13/15.
 */

import Numeric.Implicits._
import Fractional.Implicits._

trait VectorBinOps[T] {
  def +(that: Vector[T])(implicit ev: Numeric[T]): Vector[T]
  def -(that: Vector[T])(implicit ev: Numeric[T]): Vector[T]
  def *(that: Vector[T])(implicit ev: Numeric[T]): Vector[T]
  def /(that: Vector[T])(implicit ev: Fractional[T]): Vector[T]
}

trait Vector[T] {
  def toList(): List[T]
  def zeros(length: Int)(implicit ev: Numeric[T]): Vector[T]
  def toString(): String
}


object Vector {
  def apply[T](args: T*): Vector[T] = new VectorImpl[T](args.toList)

  private class VectorImpl[@specialized(Double, Int, Float, Long)T](val _data: List[T])
    extends Vector[T]
    with VectorBinOps[T] {
    def +(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 + elem._2))
    def -(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 - elem._2))
    def *(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 * elem._2))
    def /(that: Vector[T])(implicit ev: Fractional[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 / elem._2))

    def fill(length: Int, value: T): Vector[T] = new VectorImpl[T](List.fill[T](length)(value))
    def zeros(length: Int)(implicit ev: Numeric[T]): Vector[T] = fill(length, ev.zero)
    def toList(): List[T] = _data

    override def toString(): String = "Vector(" + _data.mkString(" , ") + ")"
  }

  implicit def VectorToList[T](v: Vector[T]): List[T] = v.toList
}