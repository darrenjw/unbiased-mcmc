## ar1.R
## A coupled AR(1) example

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

car1 = function(n = 500, N = 200, a = 0.99) {
    arr = array(dim = c(n,N,2))
    x = matrix(rnorm(2*N, 2*sqrt(1/(1-a*a))), ncol=2)
    arr[1, , ] = x
    w = a/(1+a*a)
    csd = sqrt(1/(1+a*a))
    for (i in 2:n) {
        arr[i,1,] = rcouple(
            function() rnorm(1, w*(arr[i-1,N,1]+arr[i-1,2,1]), csd),
            function(xp) dnorm(xp, w*(arr[i-1,N,1]+arr[i-1,2,1]), csd),
            function() rnorm(1, w*(arr[i-1,N,2]+arr[i-1,2,2]), csd),
            function(xp) dnorm(xp, w*(arr[i-1,N,2]+arr[i-1,2,2]), csd)
        )
        for (j in 2:(N-1)) {
            arr[i,j,] = rcouple(
                function() rnorm(1, w*(arr[i,j-1,1]+arr[i-1,j+1,1]), csd),
                function(xp) dnorm(xp, w*(arr[i,j-1,1]+arr[i-1,j+1,1]), csd),
                function() rnorm(1, w*(arr[i,j-1,2]+arr[i-1,j+1,2]), csd),
                function(xp) dnorm(xp, w*(arr[i,j-1,2]+arr[i-1,j+1,2]), csd)
            )
        }
        arr[i,N,] = rcouple(
            function() rnorm(1, w*(arr[i,N-1,1]+arr[i,1,1]), csd),
            function(xp) dnorm(xp, w*(arr[i,N-1,1]+arr[i,1,1]), csd),
            function() rnorm(1, w*(arr[i,N-1,2]+arr[i,1,2]), csd),
            function(xp) dnorm(xp, w*(arr[i,N-1,2]+arr[i,1,2]), csd)
        )
    }
    arr
}
    
set.seed(3)
arr = car1()
op = par(mfrow=c(1,2))
image(arr[,,1], main="Chain 1", xlab="Iteration")
image(arr[,,2], main="Chain 2", xlab="Iteration")
par(op)
image(1*((arr[,,1]-arr[,,2]) == 0.0),
      main="Uncoupled variables in red", xlab="Iteration")

op = par(mfrow=c(2,3))
for (i in 1:6) {
    arr = car1()
    image(1*((arr[,,1]-arr[,,2]) == 0.0), ylab="Space",
          main="Uncoupled variables in red", xlab="Iteration")    
}
par(op)




## eof

