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
}

trait Vector[T] extends VectorBinOps[T] {
  def toList: List[T]
  def toString: String
  def length: Int
  def dot(that: Vector[T])(implicit ev: Numeric[T]): T
  def sum(implicit ev: Numeric[T]): T

  /* Slicing and drop methods */
  def drop(n: Int): Vector[T]
  def takeRight(n: Int): Vector[T]
  def slice(start: Int, until: Int): Vector[T]
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

    def dot(that: Vector[T])(implicit ev: Numeric[T]): T = this._data.zip(that).map(el => el._1 * el._2).reduceLeft(_ + _)
    def sum(implicit ev: Numeric[T]): T = this._data.sum

    /* Slicing and drop methods */
    def drop(n: Int): Vector[T] = new VectorImpl[T](this._data.drop(n))
    def takeRight(n: Int): Vector[T] = new VectorImpl[T](this._data.takeRight(n))
    def slice(start: Int, until: Int): Vector[T] = new VectorImpl[T](this._data.slice(start, until))

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