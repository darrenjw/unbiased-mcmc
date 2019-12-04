## coupling.R
## Looking at couplings of random variables

## Maximal (gamma) coupling
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

## Simple Gaussian example
coupled = function()
    rcouple(
        function() rnorm(1,0,1),
        function(x) dnorm(x,0,1),
        function() rnorm(1,1,2),
        function(x) dnorm(x,1,2)
        )
xm = t(sapply(1:10000, function(x) coupled()))
doPlot = function(xm) {
    op = par(mfrow=c(2,2))
    hist(xm[,2],xlab="N(1,4)",col=2, main="Marginal")
    plot(xm,pch=19,col=3,xlab="N(0,1)",ylab="N(1,4)", main="Joint")
    plot(0,0)
    hist(xm[,1],xlab="N(0,1)",col=4, main="Marginal")
    par(op)
}
doPlot(xm)
message("Gamma coupling, unequal variance")
print(summary(xm))
print(var(xm))
print(cor(xm)[2,1])

## Gaussian's with the same variance
coupled = function()
    rcouple(
        function() rnorm(1,2,1),
        function(x) dnorm(x,2,1),
        function() rnorm(1,4,1),
        function(x) dnorm(x,4,1)
        )
xm = t(sapply(1:1000, function(x) coupled()))
doPlot = function(xm) {
    op = par(mfrow=c(2,2))
    hist(xm[,2],xlab="N(4,1)",col=2, main="Marginal")
    plot(xm,pch=19,col=3,xlab="N(2,1)",ylab="N(4,1)", main="Joint")
    plot(0,0)
    hist(xm[,1],xlab="N(2,1)",col=4, main="Marginal")
    par(op)
}
doPlot(xm)
message("Gamma coupling, sd=1")
print(summary(xm))
print(var(xm))
print(cor(xm)[2,1])

## Reflection maximal coupling
refcouple = function(m1, m2, sd) {
    xd = rnorm(1,0,1)
    u = runif(1,0,1)
    if (u < dnorm(xd + (m1-m2)/sd)/dnorm(xd))
        c(m1 + sd*xd, m1 + sd*xd)
    else
        c(m1 + sd*xd, m2 + -sd*xd )
}

## Previous example with a reflection maximal coupling
coupled = function()
    refcouple(2,4,1)
xm = t(sapply(1:10000, function(x) coupled()))
doPlot(xm)
message("Reflection, sd=1")
print(summary(xm))
print(var(xm))
print(cor(xm)[2,1])

coupled = function()
    refcouple(2,4,2)
xm = t(sapply(1:10000, function(x) coupled()))
doPlot = function(xm) {
    op = par(mfrow=c(2,2))
    hist(xm[,2],xlab="N(4,4)",col=2, main="Marginal")
    plot(xm,pch=19,col=3,xlab="N(2,4)",ylab="N(4,4)", main="Joint")
    plot(0,0)
    hist(xm[,1],xlab="N(2,4)",col=4, main="Marginal")
    par(op)
}
doPlot(xm)
message("Reflection, sd=2")
print(summary(xm))
print(var(xm))
print(cor(xm)[2,1])

coupled = function()
    rcouple(
        function() rnorm(1,2,2),
        function(x) dnorm(x,2,2),
        function() rnorm(1,4,2),
        function(x) dnorm(x,4,2)
        )
xm = t(sapply(1:10000, function(x) coupled()))
doPlot(xm)
message("Gamma, sd=2")
print(summary(xm))
print(var(xm))
print(cor(xm)[2,1])


## eof

