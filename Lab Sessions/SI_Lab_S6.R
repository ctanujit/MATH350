
#################### Multiplicative Congruential Method ####################

m <- 2^(31) - 1
a <- 7^5
x <- numeric(length = 1e3)
x[1] <- 7  #x0
for(i in 2:1e3){
  x[i] <- (a * x[i-1]) %% m
}
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too


####################### Mixed Congruential Methd #######################

m <- 2^(31) - 1
a <- 7^5
c <- 2^(10) - 1
x <- numeric(length = 1e3)
x[1] <- 7
for(i in 2:1e3) {
  x[i] <- (c + a * x[i-1]) %% m
}
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too


###################### Caution #############################

m <- 1e3
a <- 1
c <- 1
x <- numeric(length = 1e3)
x[1] <- 7

for(i in 2:1e3) {
  x[i] <- (c + a * x[i-1]) %% m
}
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too

###################### Solving Simple Integral #######################

set.seed(1)
repeats <- 1e4
b <- 10
a <- 5
U <- runif(repeats, min = 0, max = 1)
X <- (b - a) * U + a #R is vectorized
5* mean(3*X^2 + 5*X)


####################### Higher Dimensional Integrals ######################

set.seed(1)
repeats <- 1e4
U1 <- runif(repeats, min = 0, max = 1)
X <- (6 - 5) * U + 5 
U2 <- runif(repeats, min = 0, max = 1)  # have to generate different U2
Y <- (3 - 2) * U + 2
mean(3*X^2 * Y)

