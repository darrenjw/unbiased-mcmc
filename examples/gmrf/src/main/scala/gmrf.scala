/*
gmrf.scala
GMRF example

 */

object Gmrf {

  import UnbiasedMcmc._
  import PImageUtils._

  import breeze.stats.distributions.Gaussian
  import breeze.linalg.{Vector => BVec, _}

  val alpha = 0.99
  val sigma = 1.0

  def gibbsKernel(pi: PImage[Double]): Double = {
    val sum  = pi.up.extract + pi.down.extract + pi.left.extract + pi.right.extract
    val mean = sum / 4.0
    Gaussian(alpha * mean, sigma).draw
  }

  def oddKernel(pi: PImage[Double]): Double =
    if ((pi.x + pi.y) % 2 != 0) pi.extract else gibbsKernel(pi)
  def evenKernel(pi: PImage[Double]): Double =
    if ((pi.x + pi.y) % 2 == 0) pi.extract else gibbsKernel(pi)

  def main(args: Array[String]): Unit = {
    println(breeze.stats.distributions.Poisson(10).sample(5))

    val m0 = DenseMatrix.tabulate(300, 350) {
      case (i, j) => Gaussian(0.0, 10.0).draw
    }
    val pim0 = PImage(0, 0, BDM2I(m0))

    def pims =
      Stream.iterate(pim0)(_.coflatMap(oddKernel).coflatMap(evenKernel))

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
        p += image(I2BDM(pim.image.map { _.toDouble }))
        fig.refresh
        Thread.sleep(500)
      }
    }
    println

  }

}

// eof
