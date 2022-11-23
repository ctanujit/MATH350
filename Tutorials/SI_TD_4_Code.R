
################################ Problem 1 #####################################

### Computing the Gamma MLE. ###
# Part (i)
# In R, this function may be defined as
gammaMLE <- function(X) {
  ...
  return(c(ahat,bhat))
}
# where ... should be filled in with the code to compute the MLE estimates ahat and bhat from X. 
#You may terminate the Newton-Raphson iterations when absolute difference is sufficiently small:
#For example, the high-level organization of the code to compute αhat can be

a.prev = -Inf
a = # fill in code to initialize alpha^{(0)}
  while (abs(a-a.prev) > 1e-12) {
    # fill in code to compute alpha^{(t+1)} from alpha^{(t)} = a
    # set a.new to be alpha^{(t+1)}
    a.prev = a
    a = a.new
  }
ahat = a
# The log-gamma function, digamma function, and its derivative
# (called the trigamma function) are available in R as lgamma(alpha), digamma(alpha),
#and trigamma(alpha) respectively.


########### Solution (i) 

gamma.MLE = function(X) {
  ahat = compute.ahat(X)
  bhat = ahat/mean(X)

  return(c(ahat,bhat)) 
}

#estimateahatbyNewton-Raphson 
compute.ahat = function(X) {
  a.prev = -Inf 
  a = mean(X)^2/var(X)#initialguess 
  
  #while not converged, do Newton-Raphson update 
  while(abs(a-a.prev)>1e-12) { 
    a.prev = a 
    numerator = -f(a.prev)+log(mean(X))-mean(log(X)) 
    denominator = f.prime(a.prev) 
    a = a.prev+numerator/denominator 
  } 
  
  return(a) 
} 

#define some helper functions 
f=function(alpha) {
  
  return(log(alpha)-digamma(alpha))
} 
f.prime=function(alpha) { 
  return(1/alpha-trigamma(alpha)) 
}

#################### Part (ii)

# In R you may simulate samples from Gamma (alpha, beta) using 

X = rgamma(n , alpha, rate = beta )

# The sample variance  of a vector of values of X is given by var(X), and the sample covariance 
# between two  vectors of values X and Y (of  the same length) is given by cov(X, Y).

n=500 
n.reps=5000 
alpha=1 
beta=2 

alpha.hat=numeric(n.reps) 
beta.hat=numeric(n.reps) 

for(i in 1:n.reps) {
  X=rgamma(n,shape=alpha,rate=beta)
  estimates=gamma.MLE(X) 
  alpha.hat[i] = estimates[1] 
  beta.hat[i] = estimates[2] 
  
} 

hist(alpha.hat, breaks=20)

hist(beta.hat, breaks=20)

mean(alpha.hat) # should be close to 1
## [1] 1.003308

mean(beta.hat) # should be close to 2
## [1] 2.010126

var(alpha.hat) 
## [1] 0.003079451 

var(beta.hat) 
## [1] 0.02048946

cov(alpha.hat,beta.hat) 
## [1] 0.006190339

n*var(alpha.hat) #should be close to 1.551 
## [1] 1.539726

n*var(beta.hat) #should be close to 10.202 
## [1] 10.24473

n*cov(alpha.hat,beta.hat) #should be close to 3.101 
## [1] 3.09517

################################################################################

################################# Problem 2 ####################################

set.seed(1)
n = 100 
B = 10000 
for (mu in c(0,0.1,0.2,0.3,0.4)) {
  output.Z = numeric(B) 
  output.T = numeric(B) 
  output.W = numeric(B) 
  output.S = numeric(B) 
  for (i in 1:B) { 
    X = rnorm(n, mean=mu, sd=1) 
    if (mean(X) > 1/sqrt(n)*qnorm(0.95)) { 
      output.Z[i] = 1 
    } else { 
      output.Z[i] = 0
    } 
    T = t.test(X)$statistic 
    if (T > qt(0.95,df=n-1)) { 
      output.T[i] = 1 
    } else {
      output.T[i] = 0 
    }
    W = wilcox.test(X)$statistic 
    if (W > n*(n+1)/4+sqrt(n*(n+1)*(2*n+1)/24)*qnorm(0.95)) {
      output.W[i] = 1 
    } else {
      output.W[i] = 0 
    } 
    S = length(which(X>0)) 
    if (S > n/2+sqrt(n/4)*qnorm(0.95)) {
      output.S[i] = 1 
    } else {
      output.S[i] = 0 
    } 
  } 
  print(paste("mu = ", mu)) 
  print(paste("Z: ", mean(output.Z))) 
  print(paste("T: ", mean(output.T))) 
  print(paste("W: ", mean(output.W))) 
  print(paste("S: ", mean(output.S)))
}

################################################################################

################################# Problem 3 ####################################

ns = c(10,40,100)
ps = c(0.1,0.3,0.5) 
B=100000 
z = qnorm(0.975) 
for (n in ns) { 
  for (p in ps) { 
    cover_A = numeric(B)
    cover_B = numeric(B) 
    for (i in 1:B) { 
      phat = rbinom(1,n,p)/n 
      U = phat+z*sqrt(phat*(1-phat)/n) 
      L = phat-z*sqrt(phat*(1-phat)/n) 
      if (p <= U && p >= L) { 
        cover_A[i] = 1 
      } else {
          cover_A[i] = 0 
      }  
      U = (phat+z^2/(2*n)+z*sqrt(phat*(1-phat)/n+z^2/(4*n^2)))/(1+z^2/n) 
      L = (phat+z^2/(2*n)-z*sqrt(phat*(1-phat)/n+z^2/(4*n^2)))/(1+z^2/n) 
      if (p <= U && p >= L) { 
        cover_B[i] = 1 
      } else { 
          cover_B[i] = 0 
      } 
    }
    print(c(n,p,mean(cover_A),mean(cover_B))) 
  } 
}

################################################################################

################################# Problem 4 ####################################

n = 500 
B = 10000 
lamb_hat = numeric(B) 
se_Fisher = numeric(B) 
se_sandwich = numeric(B) 
for(i in 1:B){ 
  X = rgamma(n,2,rate=1) 
  lamb_hat[i] = 1/mean(X) 
  se_Fisher[i] = 1/(mean(X)*sqrt(n)) 
  se_sandwich[i] = sd(X)/(mean(X)^2*sqrt(n)) 
  } 
print(mean(lamb_hat)) 
print(sd(lamb_hat)) 
hist(se_Fisher) 
hist(se_sandwich)

################################################################################

################################ Problem 5 #####################################

ks = seq(0,12) 
counts = c(7,45,181,478,829,1112,1343,1033,670,286,104,24,3) 
expected = 6115*choose(12,ks)*(0.5^12) 
T1_obs = sum(ks*counts)/6115
T2_obs = sum((counts-expected)^2/expected) 
T1s = numeric(1000) 
T2s = numeric(1000)
for (i in 1:1000) {
  X = rbinom(6115, 12, 0.5) 
  T1s[i] = mean(X) 
  counts = numeric(13) 
  for (k in 0:12) {
    counts[k+1] = length(which(X==k)) 
    } 
  T2s[i] = sum((counts-expected)^2/expected) 
  } 
hist(T1s) 
hist(T2s) 
T1_pvalue = length(which(T1s<T1_obs))/1000 * 2 
T2_pvalue = length(which(T2s>T2_obs))/1000

################################################################################
