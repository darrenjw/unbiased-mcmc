# Unbiased MCMC with couplings

Yesterday there was an RSS Read Paper meeting for the paper [Unbiased Markov chain Monte Carlo with couplings](https://arxiv.org/abs/1708.03625) by Pierre Jacob, John O'Leary and Yves F. Atchadé. The paper addresses the bias in MCMC estimates due to lack of convergence to equilibrium (the "burn-in" problem), and shows how it is possible to modify MCMC algorithms in order to construct estimates which exactly remove this bias. The requirement is to couple a pair of MCMC chains so that they will at some point meet exactly and thereafter remain coupled. This turns out to be easier to do that one might naively expect. There are many reasons why we might want to remove bias from MCMC estimates, but the primary motivation in the paper was the application to parallel MCMC computation. The idea here is that many pairs of chains can be run independently on any available processors, and the unbiased estimates from the different pairs can be safely averaged to get an (improved) unbiased estimate based on all of the chains. As a discussant of the paper, I've spent a bit of time thinking about this idea, and have created a [small repository of materials](https://github.com/darrenjw/unbiased-mcmc) relating to the paper which may be useful for others interested in understanding the method and how to use it in practice.

The repo includes a [page of links](https://github.com/darrenjw/unbiased-mcmc/blob/master/Links.md) to related papers, blog posts, software and other resources relating to unbiased MCMC that I've noticed on-line.

Earlier in the year I gave an internal seminar at Newcastle giving a tutorial introduction to the main ideas from the paper, including runnable R code implementations of the examples. The talk was prepared as an executable R Markdown document. The [R Markdown source code](https://github.com/darrenjw/unbiased-mcmc/tree/master/intro) is available in the repo, but for the convenience of casual browsers I've also included a pre-built set of [PDF slides](https://github.com/darrenjw/unbiased-mcmc/raw/master/intro/UnbiasedMCMC-slides-PREBUILT.pdf). Code examples include code for maximal coupling of two (univariate) distributions, coupling Metropolis-Hastings chains, and coupling a Gibbs sampler for an AR(1) process.

I haven't yet finalised my written discussion contribution, but the slides I presented at the Read Paper meeting are also available. Again, there is [source code](https://github.com/darrenjw/unbiased-mcmc/tree/master/discussion) and pre-built [PDF slides](https://github.com/darrenjw/unbiased-mcmc/raw/master/discussion/djw-slides-PREBUILT.pdf). My discussion focused on seeing how well the technique works for Gibbs samplers applied to high-dimensional latent process models (an AR(1) process and a Gaussian Markov random field), and reflecting on the extent to which the technique really solves the [burn-in/parallel MCMC](https://darrenjw.wordpress.com/2010/12/14/getting-started-with-parallel-mcmc/) problem.

The repo also contains a few stand-alone [code examples](https://github.com/darrenjw/unbiased-mcmc/tree/master/examples). There are some simple tutorial examples in R (largely derived from my tutorial introduction), including implementation of (univariate) [independent and reflection maximal couplings](https://github.com/darrenjw/unbiased-mcmc/blob/master/examples/coupling/), and a [coupled AR(1) process](https://github.com/darrenjw/unbiased-mcmc/tree/master/examples/ar1) example.

The more substantial example concerns a [coupled Gibbs sampler for a GMRF](https://github.com/darrenjw/unbiased-mcmc/tree/master/examples/gmrf). This example is written in the Scala programming language. There are a couple of notable features of this implementation. First, the code illustrates [monadic](https://darrenjw.wordpress.com/2016/04/15/first-steps-with-monads-in-scala/) coupling of probability distributions, based on the `Rand` type in the [Breeze scientific library](https://darrenjw.wordpress.com/2013/12/30/brief-introduction-to-scala-and-breeze-for-statistical-computing/). This provides an elegant way to max couple arbitrary (continuous) random variables, and to create coupled Metropolis(-Hastings) kernels. For example, a coupling of two distributions can be constructed as
```scala
  def couple[T](p: ContinuousDistr[T], q: ContinuousDistr[T]): Rand[(T, T)] = {
    def ys: Rand[T] =
      for {
        y  <- q
        w  <- Uniform(0, 1)
        ay <- if (math.log(w) > p.logPdf(y) - q.logPdf(y)) Rand.always(y) else ys
      } yield ay
    val pair = for {
      x <- p
      w <- Uniform(0, 1)
    } yield (math.log(w) <= q.logPdf(x) - p.logPdf(x), x)
    pair flatMap {
      case (b, x) => if (b) Rand.always((x, x)) else (ys map (y => (x, y)))
    }
  }
```
and then `draws` can be sampled from the resulting `Rand[(T, T)]` polymorphic type as required. Incidentally, this also illustrates how to construct an independent maximal coupling without evaluating any raw likelihoods.
The other notable feature of the code is the use of a [parallel comonadic image type](https://darrenjw.wordpress.com/2018/01/22/comonads-for-scientific-and-statistical-computing-in-scala/) for parallel Gibbs sampling of the GMRF, producing a (lazy) `Stream` of coupled MCMC samples. 
