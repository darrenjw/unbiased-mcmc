/*
coupled-gmrf.scala
Coupled GMRF example

 */

object CoupledGmrf {

  import UnbiasedMcmc._
  import PImageUtils._

  import breeze.stats.distributions.{Rand, Gaussian}
  import breeze.linalg.{Vector => BVec, _}

  import cats._
  import cats.implicits._

  val alpha = 0.99
  val sigma = 1.0

  def gibbsKernel(pi: PImage[(Double, Double)]): Rand[(Double, Double)] = {
    val sum = pi.up.extract |+| pi.down.extract |+| pi.left.extract |+|
      pi.right.extract
    val mean = (sum._1 / 4.0, sum._2 / 4.0)
    couple(Gaussian(alpha * mean._1, sigma), Gaussian(alpha * mean._2, sigma))
  }

  def oddKernel(pi: PImage[(Double, Double)]): Rand[(Double, Double)] =
    if ((pi.x + pi.y) % 2 != 0) Rand.always(pi.extract) else gibbsKernel(pi)
  def evenKernel(pi: PImage[(Double, Double)]): Rand[(Double, Double)] =
    if ((pi.x + pi.y) % 2 == 0) Rand.always(pi.extract) else gibbsKernel(pi)

  def plotFrames(pis: Stream[PImage[Double]]): Unit = {
    import breeze.plot._
    val fig = Figure("Gibbs sampler for a GMRF")
    fig.width = 1000
    fig.height = 800
    pis.zipWithIndex.foreach {
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

  // Expects a stream containing 1.0 for coupled and 0.0 for uncoupled pixels
  def couplingTime(pis: Stream[PImage[Double]]): Int = {
    pis.zipWithIndex.dropWhile(pim => min(I2BDM(pim._1.image)) < 0.5).head._2
  }

  def main(args: Array[String]): Unit = {
    val m0 = DenseMatrix.tabulate(100, 100) {
      case (i, j) => (Gaussian(0.0, 10.0).draw, Gaussian(0.0, 10.0).draw)
    }
    val pim0 = PImage(0, 0, BDM2I(m0))
    def pims =
      Stream.iterate(pim0)(
        _.coflatMap(oddKernel).map(_.draw).coflatMap(evenKernel).map(_.draw)
      )

    def pims1 = pims.map(_.map(_._1))
    //plotFrames(pims1.take(20))

    def cpims = pims map (_.map { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 })
    //plotFrames(cpims.take(40))

    println(couplingTime(cpims))

    val times = DenseVector.fill(10)(couplingTime(cpims))
    println(times)

  }

}

// eof
