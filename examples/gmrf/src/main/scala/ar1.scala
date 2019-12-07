/*
ar1.scala
AR(1) example

 */

object Ar1 {

  import UnbiasedMcmc._

  def main(args: Array[String]): Unit = {
    println(breeze.stats.distributions.Poisson(10).sample(5))
  }

}

// eof
