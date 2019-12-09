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
  //val alpha = 1.0
  val sigma = 1.0

  def gibbsKernel(pi: PImage[(Double, Double)]): Rand[(Double, Double)] = {
    val sum = pi.up.extract |+| pi.down.extract |+| pi.left.extract |+|
      pi.right.extract
    val mean = (sum._1 / 4.0, sum._2 / 4.0)
    //couple(Gaussian(alpha * mean._1, sigma), Gaussian(alpha * mean._2, sigma))
    rmcouple(alpha * mean._1, alpha * mean._2, sigma)
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
        fig.saveas(f"frame-$i%04d.png")
        Thread.sleep(800)
      }
    }
    println
  }

  // Expects a stream containing 1.0 for coupled and 0.0 for uncoupled pixels
  def couplingTime(pis: Stream[PImage[Double]]): Int = {
    pis.zipWithIndex.dropWhile(pim => min(I2BDM(pim._1.image)) < 0.5).head._2
  }

  // Expects a vector of coupling times
  def summariseTimes(times: DenseVector[Int], plot: Boolean = true): Unit = {
    println(times)
    val dtimes = times map (_.toDouble)
    import breeze.stats._
    val mav = meanAndVariance(dtimes)
    println(mav)
    print("Median: " + median(dtimes) + ", ")
    println("Max: " + max(dtimes) + ", Min: " + min(dtimes))
    if (plot) {
      import breeze.plot._
      val fig = Figure("Histograms")
      fig.clear
      val p = fig.subplot(1, 1, 0)
      p.title = s"Histogram of coupling times for a GMRF"
      p += hist(dtimes, 20)
      fig.refresh
      fig.saveas("histogram.png")
    }
  }

  // Main runner function
  def main(args: Array[String]): Unit = {
    // val m0 = DenseMatrix.tabulate(150, 150) {
    val m0 = DenseMatrix.tabulate(50, 50) {
      case (i, j) => (Gaussian(0.0, 10.0).draw, Gaussian(0.0, 10.0).draw)
    }
    val pim0 = PImage(0, 0, BDM2I(m0))
    def pims =
      Stream.iterate(pim0)(
        _.coflatMap(oddKernel).map(_.draw).coflatMap(evenKernel).map(_.draw)
      )
    def pims1 = pims.map(_.map(_._1)) // First marginal component
    //plotFrames(pims1.take(50))
    def cpims = pims map (_.map { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 })
    //plotFrames(cpims.take(150)) // Stream of coupled pixels
    //println(couplingTime(cpims)) // A single coupling time
    println("Computing coupling time distribution - this could take a while...")
    val times = DenseVector.fill(100)(couplingTime(cpims))
    summariseTimes(times)
  }

}

// eof
