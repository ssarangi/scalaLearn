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

/**
 * Created by sarangis on 7/13/15.
 * Adapted from this blog: https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
 * as well as http://etrain.github.io/2015/05/28/type-safe-linear-algebra-in-scala/
 */

import Numeric.Implicits._

trait MatrixBinOps[T] {
  def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
  def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
  def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T]
}

trait Matrix[T] extends MatrixBinOps[T] {
  def isEmpty: Boolean
  def transpose: Matrix[T]
  def row(i: Int)(implicit ev: Numeric[T]): Vector[T]
  def col(i: Int)(implicit ev: Numeric[T]): Vector[T]
  def insert_row(col_to_insert: Int, v: Vector[T]): Matrix[T]
  def insert_col(col_to_insert: Int, v: Vector[T]): Matrix[T]

  def add_row(v: Vector[T]): Matrix[T]
  def add_col(v: Vector[T]): Matrix[T]

  def rows: Int
  def cols: Int

  def toList: List[List[T]]
}

object Matrix {

  def apply[T](args: List[T]*): Matrix[T] = new MatrixImpl(args.toList)
  def empty[T]: Matrix[T] = new MatrixImpl[T](Nil)

  def zeros[T](m: Int)(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(m)(em.zero)))
  def zeros[T](m: Int, n: Int)(implicit ev: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(n)(ev.zero)))

  def identity[T](m: Int)(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl(Matrix.map(m , m) {(i: Int, j: Int) => if (i == j) em.one else em.zero })

  def map[T](rowCount:Int, colCount:Int)(f:(Int,Int) => T) =
    (
      for(i <- 1 to rowCount) yield
      (for(j <- 1 to colCount) yield f(i,j)).toList
      ).toList

  private class MatrixImpl[@specialized(Double, Int, Float, Long) T](val _data: List[List[T]]) extends Matrix[T] {

    /**
     * Recursive transpose method: Private to the class and does a recursive transpose by taking in the internals of matrix
     * @param ll: Internal representation of the matrix
     * @return List[List[T]]: Returns a transposed matrix
     */
    private def recursive_transpose(ll: List[List[T]]): List[List[T]] = if (ll.isEmpty || ll.head.isEmpty) Nil else ll.map(_.head) :: recursive_transpose(ll.map(_.tail))

    /**
     * Recursively insert a column to the matrix. Current assumptions are that the matrix is not empty.
     * @param start_col: This is the column where we want to start checking for. start_col < col_to_insert
     * @param col_to_insert: This is where the new column should be inserted
     * @param v: The column which gets inserted
     * @param ll: The internal representation of the matrix
     * @return List[List[T]]: Returns a new matrix with the column inserted
     */
    private def recursive_insert_col(start_col: Int, col_to_insert: Int, v: List[T], ll: List[List[T]]): List[List[T]] = {
      if (ll.head.isEmpty) Nil
      else if (start_col == col_to_insert) v :: ll.map(_.head) :: recursive_insert_col(start_col + 1, col_to_insert, v, ll.map(_.tail))
      else ll.map(_.head) :: recursive_insert_col(start_col + 1, col_to_insert, v, ll.map(_.tail))
    }

    val rows: Int = if (_data.isEmpty || _data.head.isEmpty) 0 else _data.length
    val cols: Int = if (_data.isEmpty || _data.head.isEmpty) 0 else _data.head.length

    def isEmpty: Boolean = _data.isEmpty

    def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that.transpose).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def transpose(): Matrix[T] = new MatrixImpl[T](recursive_transpose(_data))

    def add_row(v: Vector[T]): Matrix[T] = new MatrixImpl[T](if (isEmpty) List(v.toList) else this ++ List(v.toList))
    def add_col(v: Vector[T]): Matrix[T] = new MatrixImpl[T](this.transpose ++ List(v.toList)).transpose

    def row(i: Int)(implicit ev: Numeric[T]): Vector[T] = _data(i)
    def col(i: Int)(implicit ev: Numeric[T]): Vector[T] = recursive_transpose(_data)(i)

    def insert_col(col_to_insert: Int, v: Vector[T]): Matrix[T] = new MatrixImpl[T](recursive_insert_col(0, col_to_insert, v, _data)).transpose
    def insert_row(row_to_insert: Int, v: Vector[T]): Matrix[T] = new MatrixImpl[T](recursive_insert_col(0, row_to_insert, v, this.transpose))

    def map[T]()(f:(Int,Int) => T) =
      (
        for(i <- 1 to this.rows) yield
        (for(j <- 1 to this.cols) yield f(i,j)).toList
        ).toList

    def toList: List[List[T]] = _data

    override def toString: String = "Matrix" + _data.mkString("[", ",\n       ", "]").replace("List", "")
  }

  implicit def MatrixToList[T](m: Matrix[T]): List[List[T]] = m.toList

}