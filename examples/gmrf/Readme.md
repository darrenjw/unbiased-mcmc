# GMRF example

## Coupling a Gibbs sampler for a Gaussian Markov random field

This example is written in the Scala programming language, and is intended to be built and run using the Scala build tool, sbt. All you need to build and run the examples is a reasonably recent JDK (eg. `openjdk-8-jdk` on Debian and related Linux distros) and [sbt](https://www.scala-sbt.org/). Note that you do *not* need a system-wide Scala installation.

Just typing `sbt run` from *this* directory should build and run the project.

The code is in [./src/main/scala/](src/main/scala/).

There are a few different programs you can run:

1. CoupledAr1 - coupling time distribution for a Gibbs sampler of a linear Gaussian AR(1) process
2. CoupledGmrf - coupling time distribution for a Gibbs sampler of a GMRF
3. CouplingExamples - some simple coupling examples
4. Gmrf - just a simple uncoupled simulation of a GMRF, to illustrate the comonadic approach to Gibbs sampling

