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

object PImageUtils {

  // Helpers for packing and unpacking
  import breeze.linalg.{Vector => BVec, _}
  def BDM2I[T](m: DenseMatrix[T]): Image[T] =
    Image(m.cols, m.rows, m.data.toVector.par)
  def I2BDM(im: Image[Double]): DenseMatrix[Double] =
    new DenseMatrix(im.h, im.w, im.data.toArray)

  // Provide evidence to Cats that PImage is a Comonad (not strictly needed)
  import cats._
  import cats.implicits._
  implicit val pimageComonad = new Comonad[PImage] {
    def extract[A](wa: PImage[A]) = wa.extract
    def coflatMap[A, B](wa: PImage[A])(f: PImage[A] => B): PImage[B] =
      wa.coflatMap(f)
    def map[A, B](wa: PImage[A])(f: A => B): PImage[B] = wa.map(f)
  }

}

// eof
