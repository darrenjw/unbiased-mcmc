## test-code.R
## File for developing the examples

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
    




## eof

