
############### Empirical CDF Plot ###################

data <- rnorm(50, 0, 1) ## store 50 normal random variables in variable "data"
F50 <- ecdf(data) #f stores the empirical distribution function
plot.ecdf(F50)
data1 <- rnorm(10)
F10 <- ecdf(data1)
plot.ecdf(F10)

############### Centered Histogram ###################

hist(data, breaks = 20, col = NULL) ## to get histogram with 20 bins, high variance, 0 prob region
hist(data, breaks = 3, col = NULL) # over smoothed

x <- density(data, kernel = "r", bw = 0.1)
z <- density(data, kernel = "r", bw = 0.5)
plot(x)
plot(z)


############## Plotting KDE #######################
plot(density(data, kernel = "gaussian"))
plot(density(data, kernel = "epanechnikov")) #smoother the kernel, smoother the estimate
plot(density(data, kernel = "triangular"))
plot(density(data, kernel = "gaussian", bw= 0.3 ))
plot(density(data, kernel = "gaussian", bw= 0.6 ))

############# Smoothing #####################

##########  Friedman Local Averaging #############
library(lattice)
etoh <- lattice::ethanol #ethanol data
head(etoh)
plot(x = etoh$E , y = etoh$NOx , xlab = "engine ratio", ylab = "N oxide conc")

## data we want to smooth
#use "supsmu" to smooth the data using local averaging method
# use span = "cv" to use the cross validated variable span

plot(supsmu(x = etoh$E, y = etoh$NOx , span = "cv"))

# cv span related smoothing might have a better balance of variance and bias

# if span = p then it smooths the data using a constant span of size n

plot(supsmu(x = etoh$E, y = etoh$NOx , span = 0.05))
plot(supsmu(x = etoh$E, y = etoh$NOx , span = 0.30))

# appears to be too smoothed. So biased.

############### N-W Estimator for Ethanol data ##################

# "npreg" command implements the N-W kernel regression estimator
etoh$NOx <- etoh$NOx[order(etoh$E)]
etoh$E <- sort(etoh$E) # "npreg" requires x variable data to be sorted
library(np)
etoh.npreg <- npreg(bws = 0.09 , txdat = etoh$E , tydat = etoh$NOx)
plot(etoh.npreg)
