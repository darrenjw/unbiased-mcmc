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

## Coupled Gibbs

car1 = function(n = 500, N = 200, a = 0.99) {
    arr = array(dim = c(n,N,2))
    x = matrix(rnorm(2*N, 2*sqrt(1/(1-a*a))), ncol=2)
    arr[1, , ] = x
    w = a/(1+a*a)
    csd = sqrt(1/(1+a*a))
    for (i in 2:n) {
        arr[i, 1, ] = rcouple(
            function() rnorm(1, w*(arr[i-1,N,1]+arr[i-1,2,1]), csd),
            function(xp) dnorm(xp, w*(arr[i-1,N,1]+arr[i-1,2,1]), csd),
            function() rnorm(1, w*(arr[i-1,N,2]+arr[i-1,2,2]), csd),
            function(xp) dnorm(xp, w*(arr[i-1,N,2]+arr[i-1,2,2]), csd)
        )
        for (j in 2:(N-1)) {
            arr[i, j, ] = rcouple(
                function() rnorm(1, w*(arr[i,j-1,1]+arr[i-1,j+1,1]), csd),
                function(xp) dnorm(xp, w*(arr[i,j-1,1]+arr[i-1,j+1,1]), csd),
                function() rnorm(1, w*(arr[i,j-1,2]+arr[i-1,j+1,2]), csd),
                function(xp) dnorm(xp, w*(arr[i,j-1,2]+arr[i-1,j+1,2]), csd)
            )
        }
        arr[i, N, ] = rcouple(
            function() rnorm(1, w*(arr[i,N-1,1]+arr[i,1,1]), csd),
            function(xp) dnorm(xp, w*(arr[i,N-1,1]+arr[i,1,1]), csd),
            function() rnorm(1, w*(arr[i,N-1,2]+arr[i,1,2]), csd),
            function(xp) dnorm(xp, w*(arr[i,N-1,2]+arr[i,1,2]), csd)
        )
    }
    arr
}
    
arr = car1()
op = par(mfrow=c(1,2))
image(arr[, , 1], main="Chain 1", xlab="Iteration")
image(arr[, , 2], main="Chain 2", xlab="Iteration")
par(op)
Sys.sleep(1)
image(1*((arr[, , 1]-arr[, , 2]) == 0.0),
      main="Uncoupled variables in red", xlab="Iteration")
Sys.sleep(2)

op = par(mfrow=c(2,3))
for (i in 1:6) {
    arr = car1()
    image(1*((arr[, , 1]-arr[, , 2]) == 0.0), ylab="Space",
          main="Uncoupled variables in red", xlab="Iteration")    
}
par(op)




## eof

