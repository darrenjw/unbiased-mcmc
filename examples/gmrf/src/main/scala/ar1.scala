/*
ar1.scala
Coupled AR(1) example

 */

object CoupledAr1 {

  import UnbiasedMcmc._
  import PImageUtils._

  import breeze.stats.distributions.{Rand, Gaussian}
  import breeze.linalg.{Vector => BVec, _}

  import cats._
  import cats.implicits._

  val alpha = 0.99
  val sigma = 1.0

  val weight = alpha/(1.0+alpha*alpha)
  val csd = sigma/math.sqrt(1.0+alpha*alpha)

  def gibbsKernel(pv: PVec[(Double, Double)]): Rand[(Double, Double)] = {
    val sum = pv.left.extract |+| pv.right.extract
    couple(Gaussian(weight * sum._1, csd), Gaussian(weight * sum._2, csd))
    //rmcouple(weight * sum._1, weight * sum._2, csd)
  }

  def oddKernel(pv: PVec[(Double, Double)]): Rand[(Double, Double)] =
    if (pv.t % 2 != 0) Rand.always(pv.extract) else gibbsKernel(pv)
  def evenKernel(pv: PVec[(Double, Double)]): Rand[(Double, Double)] =
    if (pv.t % 2 == 0) Rand.always(pv.extract) else gibbsKernel(pv)

  // Expects a stream containing 1.0 for coupled and 0.0 for uncoupled pixels
  def couplingTime(pvs: Stream[PVec[Double]]): Int = {
    pvs.zipWithIndex.dropWhile(pim => pim._1.v.min < 0.5).head._2
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
      p.title = s"Histogram of coupling times for a AR(1) process"
      p += hist(dtimes, 20)
      fig.refresh
      fig.saveas("histogram.png")
    }
  }

  // Main runner function
  def main(args: Array[String]): Unit = {
    val ssd = sigma/math.sqrt(1.0-alpha*alpha)
    def v0 = Vector.fill(200)((Gaussian(0.0,ssd).draw, Gaussian(0.0,ssd).draw))
    def pv0 = PVec(0, v0.par)
    def pvs =
      Stream.iterate(pv0)(
        _.coflatMap(oddKernel).map(_.draw).coflatMap(evenKernel).map(_.draw)
      )
    def pvs1 = pvs.map(_.map(_._1)) // First marginal component
    def cpvs = pvs map (_.map { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 })
    println(couplingTime(cpvs)) // A single coupling time
    println("Computing coupling time distribution - this could take a while...")
    val times = DenseVector.fill(1000)(couplingTime(cpvs))
    summariseTimes(times)
  }

}

// eof
