/*
pimage.scala
Pointed image class

 */

// Basic rectangular "image" type
import scala.collection.parallel.immutable.ParVector
case class Image[T](w: Int, h: Int, data: ParVector[T]) {
  def apply(x: Int, y: Int): T    = data(x * h + y)
  def map[S](f: T => S): Image[S] = Image(w, h, data map f)
  def updated(x: Int, y: Int, value: T): Image[T] =
    Image(w, h, data.updated(x * h + y, value))
}

// "Pointed image": include a "cursor" pointing to "current" pixel,
//  thereby allowing implementation of "coflatMap"
case class PImage[T](x: Int, y: Int, image: Image[T]) {
  def extract: T                   = image(x, y)
  def map[S](f: T => S): PImage[S] = PImage(x, y, image map f)
  def coflatMap[S](f: PImage[T] => S): PImage[S] =
    PImage(
      x,
      y,
      Image(
        image.w,
        image.h,
        (0 until (image.w * image.h)).toVector.par.map(i => {
          val xx = i / image.h
          val yy = i % image.h
          f(PImage(xx, yy, image))
        })
      )
    )
  def up: PImage[T] = {
    val py = y - 1
    val ny = if (py >= 0) py else (py + image.h)
    PImage(x, ny, image)
  }
  def down: PImage[T] = {
    val py = y + 1
    val ny = if (py < image.h) py else (py - image.h)
    PImage(x, ny, image)
  }
  def left: PImage[T] = {
    val px = x - 1
    val nx = if (px >= 0) px else (px + image.w)
    PImage(nx, y, image)
  }
  def right: PImage[T] = {
    val px = x + 1
    val nx = if (px < image.w) px else (px - image.w)
    PImage(nx, y, image)
  }
}

// Pointed Vector for 1d problems
case class PVec[T](t: Int, v: ParVector[T]) {
  def extract: T = v(t)
  def map[S](f: T => S): PVec[S] = PVec(t, v map f)
  def coflatMap[S](f: PVec[T] => S): PVec[S] =
    PVec(t,(0 until v.length).toVector.par.map(i => f(PVec(i,v))))
  def left: PVec[T] = PVec(if (t > 0) t-1 else v.length-1, v)
  def right: PVec[T] = PVec(if (t < v.length-1) t+1 else 0, v)
}

object PImageUtils {

  // Helpers for packing and unpacking
  import breeze.linalg.{Vector => BVec, _}
  def BDM2I[T](m: DenseMatrix[T]): Image[T] =
    Image(m.cols, m.rows, m.data.toVector.par)
  def I2BDM(im: Image[Double]): DenseMatrix[Double] =
    new DenseMatrix(im.h, im.w, im.data.toArray)

  import cats._
  import cats.implicits._
  // Provide evidence to Cats that PImage is a Comonad (not strictly needed)
  implicit val pimageComonad = new Comonad[PImage] {
    def extract[A](wa: PImage[A]) = wa.extract
    def coflatMap[A, B](wa: PImage[A])(f: PImage[A] => B): PImage[B] =
      wa.coflatMap(f)
    def map[A, B](wa: PImage[A])(f: A => B): PImage[B] = wa.map(f)
  }
  // Provide evidence to Cats that PVec is a Comonad (not strictly needed)
  implicit val pvecComonad = new Comonad[PVec] {
    def extract[A](wa: PVec[A]) = wa.extract
    def coflatMap[A, B](wa: PVec[A])(f: PVec[A] => B): PVec[B] =
      wa.coflatMap(f)
    def map[A, B](wa: PVec[A])(f: A => B): PVec[B] = wa.map(f)
  }

}

// eof
