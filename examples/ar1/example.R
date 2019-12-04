####################################################################
## example.R
## A coupled AR(1) example
####################################################################

## Main coupled simulation functions in "ar1.R"
source("ar1.R")

## Examples for maximal (gamma) coupling
arr = car1()
op = par(mfrow=c(1,2))
image(arr[, , 1], main="Chain 1", xlab="Iteration")
image(arr[, , 2], main="Chain 2", xlab="Iteration")
par(op)
Sys.sleep(2)
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
Sys.sleep(2)

####################################################################

## Examples for reflection maximal coupling
arr = refcar1()
op = par(mfrow=c(1,2))
image(arr[, , 1], main="Chain 1", xlab="Iteration")
image(arr[, , 2], main="Chain 2", xlab="Iteration")
par(op)
Sys.sleep(2)
image(1*((arr[, , 1]-arr[, , 2]) == 0.0),
      main="Uncoupled variables in red", xlab="Iteration")
Sys.sleep(2)

op = par(mfrow=c(2,3))
for (i in 1:6) {
    arr = refcar1()
    image(1*((arr[, , 1]-arr[, , 2]) == 0.0), ylab="Space",
          main="Uncoupled variables in red", xlab="Iteration")    
}
par(op)




####################################################################
## eof
####################################################################

