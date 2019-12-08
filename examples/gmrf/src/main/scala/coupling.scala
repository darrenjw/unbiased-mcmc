/*
coupling.scala

Functions for creating coupled random variables, coupled Metropolis-Hastings kernels, etc.

 */

import breeze.stats.distributions._

object UnbiasedMcmc {

  // Monadic max coupling of two continuous distributions
  def couple[T](p: ContinuousDistr[T], q: ContinuousDistr[T]): Rand[(T, T)] = {
    def ys: Rand[T] =
      for {
        y  <- q
        w  <- Uniform(0, q.pdf(y))
        ay <- if (w > p.pdf(y)) Rand.always(y) else ys
      } yield ay // TODO: use tailRecM to make tail recursive
    val pair = for {
      x <- p
      w <- Uniform(0, p.pdf(x))
    } yield (w <= q.pdf(x), x)
    pair flatMap {
      case (b, x) => if (b) Rand.always((x, x)) else (ys map (y => (x, y)))
    }
  }

  // A simple (uncoupled) Metropolis kernel for target logPi
  def metKernel[T](
      q: T => ContinuousDistr[T]
  )(logPi: T => Double)(x: T): Rand[T] =
    for {
      xs <- q(x)
      u  <- Uniform(0, 1)
      nx = if (math.log(u) < logPi(xs) - logPi(x)) xs else x
    } yield nx

  // Monadic coupling of a Metropolis kernel for target logPi
  def coupledMetKernel[T](
      q: T => ContinuousDistr[T]
  )(logPi: T => Double)(x: (T, T)): Rand[(T, T)] =
    for {
      p <- couple(q(x._1), q(x._2))
      u <- Uniform(0, 1)
      n1 = if (math.log(u) < logPi(p._1) - logPi(x._1)) p._1 else x._1
      n2 = if (math.log(u) < logPi(p._2) - logPi(x._2)) p._2 else x._2
    } yield (n1, n2)

  // Monadic reflection maximal coupling of two _univariate_ Gaussians
  def rmcouple(m1: Double, m2: Double, sig: Double): Rand[(Double, Double)] = {
    val z = Gaussian(0, 1)
    for {
      xd <- z
      u  <- Uniform(0, 1)
      y = if (math.log(u) < z.logPdf(xd + (m1 - m2) / sig) - z.logPdf(xd))
        (m1 + sig * xd)
      else (m2 - sig * xd)
    } yield (m1 + sig * xd, y)
  }

}

object CouplingExamples {

  import UnbiasedMcmc._

  def couplingTest: Unit = {
    plotCoupling(couple(Gamma(10, 0.1), Gamma(10, 0.1)))
    plotCoupling(couple(Gamma(10, 0.1), Gamma(20, 0.1)))
    plotCoupling(couple(Gaussian(5, 2), Gaussian(4, 3)))
    plotCoupling(couple(Gaussian(2, 2), Gaussian(4, 2)))
    plotCoupling(rmcouple(2, 4, 2))
    plotCoupling(couple(Uniform(0, 2), Uniform(1, 4)))
  }

  def plotCoupling(c: Rand[(Double, Double)]): Unit = {
    val cs = c.sample(100000)
    import breeze.linalg._
    val x = DenseVector((cs map (_._1)).toArray)
    val y = DenseVector((cs map (_._2)).toArray)
    import breeze.plot._
    val fig = Figure("Coupling")
    val p0  = fig.subplot(1, 3, 0)
    p0 += plot(x, y, '.')
    val p1 = fig.subplot(1, 3, 1)
    p1 += hist(x, 30)
    p1.ylim = (0, p1.ylim._2)
    val p2 = fig.subplot(1, 3, 2)
    p2 += hist(y, 30)
    p2.ylim = (0, p2.ylim._2)
    import breeze.stats._
    print("x: ")
    println(meanAndVariance(x))
    print("Median: " + median(x) + ", ")
    println("Max: " + max(x) + ", Min: " + min(x))
    print("y: ")
    println(meanAndVariance(y))
    print("Median: " + median(y) + ", ")
    println("Max: " + max(y) + ", Min: " + min(y))
  }

  def metropTest: Unit = {
    val chain = Stream
      .iterate(0.0)(
        xi =>
          metKernel((x: Double) => Uniform(x - 0.5, x + 0.5))(
            x => Gaussian(0.0, 1.0).logPdf(x)
          )(xi).draw
      )
      .drop(1)
      .take(10000)
      .toArray
    import breeze.plot._
    import breeze.linalg._
    val fig = Figure("Metropolis chain")
    val p0  = fig.subplot(0)
    p0 += plot(linspace(1, chain.length, chain.length), chain)
  }

  def coupledMetropTest: Unit = {
    val chainProcess = Stream.iterate((5.0, -5.0))(
      xi =>
        coupledMetKernel((x: Double) => Uniform(x - 0.5, x + 0.5))(
          x => Gaussian(0.0, 1.0).logPdf(x)
        )(xi).draw
    )
    val chain = chainProcess
      .sliding(2)
      .takeWhile(ps => ps.head._1 != ps.head._2)
      .map(ps => ps.tail.head)
      .toArray
    val chain1 = chainProcess.takeWhile(ps => ps._1 != ps._2).toArray
    val x      = chain map (_._1)
    val y      = chain map (_._2)
    import breeze.plot._
    import breeze.linalg._
    val fig = Figure("Pair of coupled Metropolis chains")
    val p0  = fig.subplot(0)
    p0 += plot(linspace(1, x.length, x.length), x)
    p0 += plot(linspace(1, y.length, y.length), y)
  }

  def main(args: Array[String]): Unit = {
    couplingTest
    metropTest
    coupledMetropTest
  }

}
