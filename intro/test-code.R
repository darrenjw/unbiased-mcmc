## test-code.R
## File for developing the examples


## Coupled Metropolis independence samplers

set.seed(4)
n = 40
xm = matrix(0, nrow=n, ncol=2)
x = rcauchy(2)
xm[1, ] = x
for (i in 2:n) {
  xs = rcauchy(1)
  u = runif(1)
  a = dnorm(xs, 0, 0.5)*dcauchy(x) / (
        dnorm(x, 0, 0.5)*dcauchy(xs) )
  x[u < a] = xs
  xm[i, ] = x
  }

## plot(ts(xm), plot.type="single", col=c(2,3), lwd=3)

cmis = function(n, rpi0, dpi, rq, dq) {
    xm = matrix(0, nrow=n, ncol=2)
    x = c(rpi0(), rpi0())
    xm[1, ] = x
    for (i in 2:n) {
        xs = rq()
        u = runif(1)
        a = dpi(xs)*dq(x)/(dpi(x)*dq(xs))
        x[u < a] = xs
        xm[i, ] = x
    }
    xm
}

set.seed(4)
out = cmis(
    40,
    function() rcauchy(1),
    function(x) dnorm(x,0,0.5),
    function() rcauchy(1),
    function(x) dcauchy(x))
plot(ts(out), plot.type="single", col=c(2,3), lwd=3)

cmis1 = function(n, rpi0, dpi, rq, dq) {
    xm = matrix(NA, nrow=n+1, ncol=2)
    x = c(rpi0(), rpi0()); xm[1, ] = x
    xs = rq(); u = runif(1)
    a = dpi(xs)*dq(x[1])/(dpi(x[1])*dq(xs))
    if (u < a)
        x[1] = xs
    xm[2,1] = x[1]
    for (i in 2:n) {
        xs = rq(); u = runif(1)
        a = dpi(xs)*dq(x)/(dpi(x)*dq(xs))
        x[u < a] = xs
        xm[i+1, 1] = x[1]; xm[i, 2] = x[2]
    }
    xm
}

    
set.seed(4)
out = cmis1(
    40,
    function() rcauchy(1),
    function(x) dnorm(x,0,0.5),
    function() rcauchy(1),
    function(x) dcauchy(x)
)
plot(ts(out), plot.type="single", col=c(2,3), lwd=3)


## Maximal coupling

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

coupled = function()
    rcouple(
        function() rnorm(1,0,1),
        function(x) dnorm(x,0,1),
        function() rnorm(1,1,2),
        function(x) dnorm(x,1,2)
        )

xm = t(sapply(1:1000, function(x) coupled()))
doPlot = function(xm) {
    op = par(mfrow=c(2,2))
    hist(xm[,2],xlab="N(1,4)",col=2, main="First marginal")
    plot(xm,pch=19,col=3,xlab="N(0,1)",ylab="N(1,4)", main="Joint")
    plot(0,0)
    hist(xm[,1],xlab="N(0,1)",col=4, main="Second marginal")
    par(op)
}

doPlot(xm)


## Coupled MH

cmh1 = function(n, rpi0, dpi, rq, dq) {
    xm = matrix(NA, nrow=n+1, ncol=2)
    x = c(rpi0(), rpi0()); xm[1, ] = x
    xs = rq(x[1]); u = runif(1)
    a = dpi(xs)*dq(xs,x[1])/(dpi(x[1])*dq(x[1],xs))
    if (u < a)
        x[1] = xs
    xm[2,1] = x[1]
    for (i in 2:n) {
        xs = rcouple(
            function() rq(x[1]), function(xp) dq(x[1],xp),
            function() rq(x[2]), function(xp) dq(x[2],xp) )
        u = runif(1)
        a = dpi(xs)*dq(xs,x)/(dpi(x)*dq(x,xs))
        x[u < a] = xs[u < a]
        xm[i+1, 1] = x[1]; xm[i, 2] = x[2]
    }
    xm
}

set.seed(3)
out = cmh1(
    120,
    function() rcauchy(1),
    function(x) dnorm(x,0,1),
    function(x) rnorm(1,x,0.1),
    function(x,xs) dnorm(xs,x,0.1)
)
plot(ts(out), plot.type="single", col=c(2,3), lwd=3)


## Coupled Gibbs








## eof

