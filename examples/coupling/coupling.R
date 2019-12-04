############################################################
## coupling.R
## Looking at couplings of random variables
## Simple gamma coupling and a reflection maximal coupling
## All univariate, which is not the best case for reflection
## Examples in "examples.R"
############################################################


############################################################
## Maximal (gamma) coupling
############################################################
rcouple = function(rp, dp, rq, dq) {
    x = rp()
    w = runif(1,0,dp(x))
    if (w < dq(x))
        return(c(x,x))
    else repeat {
        ys = rq()
        ws = runif(1,0,dq(ys))
        if (ws > dp(ys))
            return(c(x,ys))
        }
    }


############################################################
## Reflection maximal coupling
############################################################
refcouple = function(m1, m2, sd) {
    xd = rnorm(1,0,1)
    u = runif(1,0,1)
    if (u < dnorm(xd + (m1-m2)/sd)/dnorm(xd))
        c(m1 + sd*xd, m1 + sd*xd)
    else
        c(m1 + sd*xd, m2 - sd*xd )
}



############################################################
## eof
############################################################

