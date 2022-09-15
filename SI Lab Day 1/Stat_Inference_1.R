

######################## Pooling ###############################

hist(
  replicate(
    10000,mean(rbinom(1000, 1, .54))), main="Histogram of p_hat", ylab="Frequency", 
  xlab="p_hat", prob=TRUE,breaks=50)

curve(dnorm(x, mean=.54, sd=0.016),
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


#################### Sample mean of IID uniform #########################

nreps = 10000
sample.mean = numeric(nreps)
n = 100
for (i in 1:nreps) {
  X = runif(n, min=-1, max=1)
  sample.mean[i] = mean(X)
}
hist(sample.mean,col = "lightgreen",main = "Histogram", xlab = "Sample Mean")


################# Is your friend cheating you in dice? ###############
nreps = 10000
T = numeric(nreps)
n = 500
p = c(1/6,1/6,1/6,1/6,1/6,1/6)
for (i in 1:nreps) {
  X = rmultinom(1,n,p)
  T[i] = sum((X/n-p)^2)
}
hist(T, col = "lightgoldenrod1")


########### Approximation through numerical simulation ###########

X.bar = replicate(10^5,mean(rexp(201,1/12000)))
mean(abs(X.bar-12000) <= 1.96*0.0705*12000)

mid.range <- rep(0,10^5)
for(i in 1:10^5) {
  X <- runif(100,3,7)
  mid.range[i] <- (max(X)+min(X))/2
}
quantile(mid.range,c(0.025,0.975))

mean(mid.range)
sd(mid.range)

n = 1000
data = rbinom(n, 1, .54) # true distribution, usually unknown
estimates = rep(0,999)
for(i in 1:999) {
  id = sample(1:n, n, replace = TRUE)
  estimates[i] = mean(data[id])
}
sd(estimates)
sqrt(.54*(1-.54)/1000) # true value, usually unknown


