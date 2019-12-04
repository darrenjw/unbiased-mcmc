###################################################################
## coupling-time.R
## Compute and compare the coupling time distributions
## for regular maximal (gamma) coupling and reflection maximal
## coupling on an AR(1) model
###################################################################

## Main simulation functions in "ar1.R"
source("ar1.R")

message("PLEASE NOTE: that with default settings, this script will take an (unnecessarily) long time to run")

n = 6000 # upper bound on coupling time
N = 25   # Length of AR(1) process
a = 0.98 # Auto-regressive parameter
nd = 2000 # Number of coupling times to evaluate

message("Start with maximal (gamma) coupling")
ct = vector("numeric",nd)
for (i in 1:nd) {
    arr = car1(n=n, N=N, a=a)
    dif = as.matrix(1*(arr[,,1]-arr[,,2] != 0.0))
    image(dif)
    cc = rowSums(dif)
    ct[i] = (1:n)[cc < 0.5][1]
    message(paste(ct[i]," "), appendLF=FALSE)
}
message("")
print(summary(ct))
print(sd(ct, na.rm=TRUE))
hist(ct)

message("Now reflection maximal coupling")
rct = vector("numeric",nd)
for (i in 1:nd) {
    arr = refcar1(n=n, N=N, a=a)
    dif = as.matrix(1*(arr[,,1]-arr[,,2] != 0.0))
    image(dif)
    cc = rowSums(dif)
    rct[i] = (1:n)[cc < 0.5][1]
    message(paste(ct[i]," "), appendLF=FALSE)
}
message("")
print(summary(rct))
print(sd(rct, na.rm=TRUE))
hist(rct)

boxplot(list(gamma=ct,relect=rct), main="Coupling time distributions")

###################################################################
## eof
###################################################################
