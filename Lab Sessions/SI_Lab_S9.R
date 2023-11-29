
##### Bootstrap for the Poisson model (alpha particle data) #####

# Input: Data vector X
X = 2:17
obs = c(18,28,56,105,126,146,164,161,
        123,101,74,53,23,15,9,5)
n = sum(obs)
lambda_hat = sum(X*obs)/n

# Perform 100000 bootstrap simulations (Parametric way)
B=100000
lambda_hat_star = numeric(B)
for (i in 1:B) {
  X_star = rpois(n,lambda_hat)
  lambda_hat_star[i] = mean(X_star)
}
print(lambda_hat)
print(mean(lambda_hat_star))
print(sd(lambda_hat_star))

# Perform 100000 bootstrap simulations (Nonparametric way)
B=100000
lambda_hat_star = numeric(B)
for (i in 1:B) {
  X_star = sample(X, size=n, replace=TRUE) # sampling with replacement
  lambda_hat_star[i] = mean(X_star)
}
print(lambda_hat)
print(mean(lambda_hat_star))
print(sd(lambda_hat_star))

##### Bootstrap for the Gamma distribution #####

set.seed(10)
n <- 20
a <- 0.20
my.samp <- rgamma(n, shape = a, rate = 1)
xbar <- mean(my.samp)
B <- 1e3 # number of bootstrap samples
boot.np <- numeric(length = B) # Nonparametric method
boot.p <- numeric(length = B) # Parametric method

for(b in 1:B)
{
  boot.samp.np <- sample(my.samp, replace = TRUE) # NP Bootstrap samples
  boot.np[b] <- mean(boot.samp.np) # NP Bootstrap estimator
  
  boot.samp.p <- rgamma(n, shape = xbar, rate = 1) #parametric bootstrap samples
  boot.p[b] <- mean(boot.samp.p) # P bootstrap estimator
}

# 95% Bootstrap confidence interval
quantile(boot.np, probs = c(.025, .975))  # Nonparametric
quantile(boot.p, probs = c(.025, .975)) #parametric

# 95% asymptotic CI
c(xbar - qnorm(.975)*sqrt(xbar/n), barx + qnorm(.975)*sqrt(xbar/n))
#Notice that the CIs via bootstrap are very different

# Simulate repeated estimates to construct a 95% CI
true.samp <- numeric(length = 1e4)
for(i in 1:1e4)
{
  samp <- rgamma(n, shape = a, rate = 1)
  true.samp[i] <- mean(samp)
}
quantile(true.samp, probs = c(.025, .975))

plot(density(boot.np), col = "green", xlim = c(0,1.5),
     main = "Comparing sampling densities")
lines(density(boot.p), col = "blue")
lines(density(rnorm(1e4, mean = xbar, sd = sqrt(barx/n))), col = "red")
lines(density(true.samp))
legend("topright",lty = 1, legend = c("Truth", "Nonparametric", "Parameteric", 
"Approximate normal"), col = c("black", "green", "blue", "red"))


##### Bootstrap for the Gamma distribution (For large n) #####

n <- 1000
a <- .20
my.samp <- rgamma(n, shape = a, rate = 1)

xbar <- mean(my.samp)

B <- 1e3 # number of bootstrap samples
boot.np <- numeric(length = B)
boot.p <- numeric(length = B)

for(b in 1:B)
{
  boot.samp.np <- sample(my.samp, replace = TRUE) # NP Bootstrap samples
  boot.np[b] <- mean(boot.samp.np) # NP Bootstrap estimator
  
  boot.samp.p <- rgamma(n, shape = xbar, rate = 1) #parametric bootstrap samples
  boot.p[b] <- mean(boot.samp.p) # P bootstrap estimator
}

# 95% Bootstrap confidence interval
quantile(boot.np, probs = c(.025, .975))  # nonparametric
quantile(boot.p, probs = c(.025, .975)) # parametric

# 95% asymptotic CI
c( barx - qnorm(.975)*sqrt(xbar/n), barx + qnorm(.975)*sqrt(xbar/n) )
#Notice that the CIs via bootstrap are very different

# Simulate repeated estimates to construct a 95% CI
true.samp <- numeric(length = 1e4)
for(i in 1:1e4)
{
  samp <- rgamma(n, shape = a, rate = 1)
  true.samp[i] <- mean(samp)
}
quantile(true.samp, probs = c(.025, .975))

plot(density(boot.np), col = "green", xlim = c(.14,.3),
     main = "Comparing sampling densities", ylim = c(0,30))
lines(density(boot.p), col = "blue")
lines(density(rnorm(1e4, mean = xbar, sd = sqrt(xbar/n))), col = "red")
lines(density(true.samp))
legend("topright",lty = 1, legend = c("Truth", "Nonparametric", "Parameteric", "Approximate normal"), 
       col = c("black", "green","blue","red"))
