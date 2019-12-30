# Examples

## Unbiased MCMC examples

This directory contains some example implementations of unbiased MCMC (really, just coupling Gibbs samplers). The directory [coupling](coupling/) contains some simple maximal coupling code in R, and [ar1](ar1/) contains an example in R of using the coupling in order to couple a pair of Gibbs samplers for an AR(1) model. Some rather naive, inefficient code is included for sampling from the coupling time distribution, but this is not intended for serious use...

The directory [gmrf](gmrf/) contains more efficient code, written in Scala, for coupling, and illustrative examples showing how to couple a pair of Gibbs samplers for both the AR(1) process considered above, and a GMRF model. Code for sampling from the coupling time distribution is included. The code illustrates a functional/monadic approach to structuring coupling codes, and a comonadic approach to the implementation of Gibbs samplers.







