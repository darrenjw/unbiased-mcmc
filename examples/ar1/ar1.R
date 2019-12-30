####################################################################
## ar1.R
## A coupled AR(1) example
## Main simulation functions
####################################################################

## Maximal coupling functions
source("../coupling/coupling.R")


## TODO: currently no offset of 1...

####################################################################
## Coupled Gibbs for an AR(1) using a maximal (gamma) coupling
####################################################################
car1 = function(n = 500, N = 200, a = 0.99) {
    arr = array(dim = c(n,N,2))
    x = matrix(rnorm(2*N, 0, sqrt(1/(1-a*a))), ncol=2)
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


####################################################################
## Coupled Gibbs for an AR(1) using a reflection maximal coupling
####################################################################
refcar1 = function(n = 500, N = 200, a = 0.99) {
    arr = array(dim = c(n,N,2))
    x = matrix(rnorm(2*N, 0, sqrt(1/(1-a*a))), ncol=2)
    arr[1, , ] = x
    w = a/(1+a*a)
    csd = sqrt(1/(1+a*a))
    for (i in 2:n) {
        arr[i, 1, ] = refcouple(w*(arr[i-1,N,1]+arr[i-1,2,1]),
            w*(arr[i-1,N,2]+arr[i-1,2,2]), csd)
        for (j in 2:(N-1)) {
            arr[i, j, ] = refcouple(w*(arr[i,j-1,1]+arr[i-1,j+1,1]),
                w*(arr[i,j-1,2]+arr[i-1,j+1,2]), csd)
        }
        arr[i, N, ] = refcouple(w*(arr[i,N-1,1]+arr[i,1,1]), 
            w*(arr[i,N-1,2]+arr[i,1,2]), csd)
    }
    arr
}



####################################################################
## eof
####################################################################

