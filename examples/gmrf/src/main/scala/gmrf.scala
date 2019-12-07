/*
gmrf.scala
GMRF example

 */

object Gmrf {

  import UnbiasedMcmc._
  import PImageUtils._

  import breeze.stats.distributions.{Rand, Gaussian}
  import breeze.linalg.{Vector => BVec, _}

  val alpha = 0.99
  val sigma = 1.0

  def gibbsKernel(pi: PImage[Double]): Rand[Double] = {
    val sum = pi.up.extract + pi.down.extract + pi.left.extract +
      pi.right.extract
    val mean = sum / 4.0
    Gaussian(alpha * mean, sigma)
  }

  def oddKernel(pi: PImage[Double]): Rand[Double] =
    if ((pi.x + pi.y) % 2 != 0) Rand.always(pi.extract) else gibbsKernel(pi)
  def evenKernel(pi: PImage[Double]): Rand[Double] =
    if ((pi.x + pi.y) % 2 == 0) Rand.always(pi.extract) else gibbsKernel(pi)

  def main(args: Array[String]): Unit = {
    val m0 = DenseMatrix.tabulate(300, 350) {
      case (i, j) => Gaussian(0.0, 10.0).draw
    }
    val pim0 = PImage(0, 0, BDM2I(m0))
    def pims =
      Stream.iterate(pim0)(
        _.coflatMap(oddKernel).map(_.draw).coflatMap(evenKernel).map(_.draw)
      )

    import breeze.plot._
    val fig = Figure("Gibbs sampler for a GMRF")
    fig.width = 1000
    fig.height = 800
    pims.take(50).zipWithIndex.foreach {
      case (pim, i) => {
        print(s"$i ")
        fig.clear
        val p = fig.subplot(1, 1, 0)
        p.title = s"GMRF: frame $i"
        p += image(I2BDM(pim.image))
        fig.refresh
        Thread.sleep(500)
      }
    }
    println

  }

}

// eof
