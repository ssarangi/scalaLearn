package linalg

import linalg.LinAlgTypes.Row

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

trait Matrix[T] extends MatrixBinOps[T] {
  def isEmpty: Boolean
  def transpose: Matrix[T]
  def row(i: Int)(implicit ev: Numeric[T]): Vector[T]
  def col(i: Int)(implicit ev: Numeric[T]): Vector[T]
  def insert_row(col_to_insert: Int, v: Vector[T]): Matrix[T]
  def insert_col(col_to_insert: Int, v: Vector[T]): Matrix[T]

  def add_row(v: Vector[T]): Matrix[T]
  def add_col(v: Vector[T]): Matrix[T]

  def toList: List[List[T]]
}

object Matrix {

  def apply[T](args: List[T]*): Matrix[T] = new MatrixImpl(args.toList)
  def empty[T]: Matrix[T] = new MatrixImpl[T](Nil)

  def map[T](rowCount:Int, colCount:Int)(f:(Int,Int) => T) =
      (
        for(i <- 1 to rowCount) yield
        (for(j <- 1 to colCount) yield f(i,j)).toList
      ).toList

  def zeros[T](m: Int)(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(m)(em.zero)))
  def zeros[T](m: Int, n: Int)(implicit ev: Numeric[T]): Matrix[T] = new MatrixImpl(List.fill(m)(List.fill(n)(ev.zero)))

  def identity[T](m: Int)(implicit em: Numeric[T]): Matrix[T] = new MatrixImpl(Matrix.map(m , m) {(i: Int, j: Int) => if (i == j) em.one else em.zero })

  private class MatrixImpl[@specialized(Double, Int, Float, Long) T](val _data: List[List[T]]) extends Matrix[T] {
    def isEmpty: Boolean = _data.isEmpty

    def +(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def -(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def *(that: Matrix[T])(implicit em: Numeric[T]): Matrix[T] =
      new MatrixImpl((this, that.transpose).zipped.map((v1, v2) => (v1, v2).zipped.map(_ + _)))

    def _transpose(ll: List[List[T]]): List[List[T]] = if (ll.isEmpty || ll.head.isEmpty) Nil else ll.map(_.head) :: _transpose(ll.map(_.tail))
    def transpose(): Matrix[T] = new MatrixImpl[T](_transpose(_data))

    def add_row(v: Vector[T]): Matrix[T] = new MatrixImpl[T](if (isEmpty) List(v.toList) else this ++ List(v.toList))
    def add_col(v: Vector[T]): Matrix[T] = new MatrixImpl[T](this.transpose ++ List(v.toList)).transpose

    def row(i: Int)(implicit ev: Numeric[T]): Vector[T] = Vector[T](_data(i))
    def col(i: Int)(implicit ev: Numeric[T]): Vector[T] = Vector[T](_transpose(_data)(i))

    def _insert_col(start_col: Int, col_to_insert: Int, v: List[T], ll: List[List[T]]): List[List[T]] = {
      if (ll.head.isEmpty) List(v)
      else if (start_col == col_to_insert) v :: ll.map(_.head) :: _insert_col(start_col + 1, col_to_insert, v, ll.map(_.tail))
      else ll.map(_.head) :: _insert_col(start_col + 1, col_to_insert, v, ll.map(_.tail))
    }

    def insert_col(col_to_insert: Int, v: Vector[T]): Matrix[T] = new MatrixImpl[T](_insert_col(0, col_to_insert, v, _data)).transpose
    def insert_row(row_to_insert: Int, v: Vector[T]): Matrix[T] = new MatrixImpl[T](_insert_col(0, row_to_insert, v, this.transpose))

    def toList: List[List[T]] = _data

    override def toString: String = "Matrix" + _data.mkString("[", ",\n       ", "]").replace("List", "")
  }

  implicit def MatrixToList[T](m: Matrix[T]): List[List[T]] = m.toList

}