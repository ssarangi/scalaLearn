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
  def *(that: T)(implicit ev: Numeric[T]): Vector[T]
  def /(that: Vector[T])(implicit ev: Fractional[T]): Vector[T]
}

trait Vector[T] extends VectorBinOps[T] {
  def toList: List[T]
  def toString: String
  def length: Int
  def dot(that: Vector[T])(implicit ev: Numeric[T]): T
}

object Vector {
  def apply[T](args: T*): Vector[T] = new VectorImpl[T](args.toList)
  def apply[T](arg: List[T]): Vector[T] = new VectorImpl[T](arg)

  def zeros[T](length: Int)(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](List.fill(length)(ev.zero))

  private class VectorImpl[@specialized(Double, Int, Float, Long)T](val _data: List[T])
    extends Vector[T] {
    def +(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 + elem._2))
    def -(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 - elem._2))
    def *(that: Vector[T])(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 * elem._2))
    def *(that: T)(implicit ev: Numeric[T]): Vector[T] = new VectorImpl[T](List.fill(_data.length)(that)) * this
    def /(that: Vector[T])(implicit ev: Fractional[T]): Vector[T] = new VectorImpl[T](_data.zip(that).map(elem => elem._1 / elem._2))

    def dot(that: Vector[T])(implicit ev: Numeric[T]): T = this._data.zip(that).map(el => el._1 * el._2).reduceLeft(_ + _)

    def length: Int = _data.length
    override def toList: List[T] = this._data.toList
    override def toString: String = "Vector(" + _data.mkString(" , ") + ")"
  }

  implicit def VectorToList[T](v: Vector[T]): List[T] = v.toList
  // implicit def NumToVec[T](n: T, v: Vector[T]): Vector[T] = Vector(List.fill[T](v.length)(n))
  implicit class NumMulVector[T](val value: T) extends AnyVal {
    def +(vec: Vector[T])(implicit ev: Numeric[T]): Vector[T] = Vector(List.fill[T](vec.length)(value)) + vec
    def -(vec: Vector[T])(implicit ev: Numeric[T]): Vector[T] = Vector(List.fill[T](vec.length)(value)) - vec
    def *(vec: Vector[T])(implicit ev: Numeric[T]): Vector[T] = Vector(List.fill[T](vec.length)(value)) * vec
  }
}