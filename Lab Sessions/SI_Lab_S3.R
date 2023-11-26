

############## Multivariate Normal ##############
install.packages("mvtnorm")
library(mvtnorm)

######################### Simulating data ##########################

rnorm(10)   # generate 10 random samples from standard normal distribution
# Generate 1000 samples from a 3 dimensional normal distribution

mu1 <- c(1, 2, -5)
sigma1 <- matrix(c(1,1,0,
                   1,2,0,
                   0,0,5),3,3)
set.seed(34)
sim_mv = rmvnorm(n = 1000, mean = mu1, sigma = sigma1)
install.packages("corrplot")
library("corrplot")
corrplot(cor(sim_mv), 
         method="ellipse")

# Density using dmvnorm function

mu1 <- c(1, 2)
sigma1 <- matrix(c(1, .5, .5, 2), 2)
dmvnorm(x = c(0, 0), mean = mu1, sigma = sigma1) #0.03836759


# Density at multiple points using dmvnorm function
x <- rbind(c(0, 0), c(1, 1), c(0, 1)); x
mu1 <- c(1, 2)
sigma1 <- matrix(c(1, .5, .5, 2), 2)
dmvnorm(x = x, mean = mu1, sigma = sigma1) #0.03836759 0.09041010 0.06794114

######################### Plotting bivariate densities ######################
dnorm(1.5)  # computing density 

# Create grid
d <- expand.grid(seq(-3, 6, length.out = 50 ), seq(-3, 6, length.out = 50))
# Calculate density on grid
dens1 <- dmvnorm(as.matrix(d), mean=c(1,2), sigma=matrix(c(1, .5, .5, 2), 2))
# Convert to matrix
dens1 <- matrix(dens1, nrow = 50 )
# Use perspective plot
persp(dens1, theta = 80, phi = 30, expand = 0.6, shade = 0.2,
      col = "plum1"
      , xlab = "x"
      , ylab = "y"
      , zlab = "dens",
      main = "Theta: 80 & Phi: 30")

# Change theta and phi of perspective plot
persp(dens1, theta = 30, phi = 30, expand = 0.6, shade = 0.2,
      col = "plum1"
      , xlab = "x"
      , ylab = "y"
      , zlab = "dens",
      main = "Theta: 30 & Phi: 30")

# Change theta and phi of perspective plot
persp(dens1, theta = 80, phi = 10, expand = 0.6, shade = 0.2,
      col = "plum1"
      , xlab = "x"
      , ylab = "y"
      , zlab = "dens",
      main = "Theta: 80 & Phi: 10")


############################# Calculating CDF and Inverse CDF #############################

pnorm(200, mean = 210, sd = 10) #0.1586553
qnorm( p = 0.95, mean = 210, sd = 10) #226.4485

## Cumulative distribution

mu1 <- c(1, 2)
sigma1 <- matrix(c(1, 0.5, 0.5, 2), 2)
pmvnorm(upper = c(2, 4), mean = mu1, sigma = sigma1)

## probability between two values 1<x<2 and 2<y<4
pmvnorm(lower = c(1, 2),
        upper = c(2, 4),
        mean = mu1,
        sigma = sigma1) # 0.1627911


################################### Calculating quantiles using qmvnorm #####################################
sigma1 <- diag(2)
sigma1
qmvnorm(p = 0.95, sigma = sigma1,
        tail = "both") #2.236422

################################# Checking Normality ###########################################
data(iris)
# univariate normality tests
qqnorm(iris[, 1])
qqline(iris[, 1])


# Multivariate normality test

# qqnorm for all variables in iris data
install.packages("MVN")
library(MVN)
mvn(iris[, 1:4],
    subset = NULL,
    mvnTest = "mardia") # Mardia test

# Checking QQ plot with Mardia test
mvn(iris[, 1:4],
    subset = NULL,
    mvnTest = "mardia", multivariatePlot = "qq")

# hzTest to check multivariate normality
mvn(iris[, 1:4],
    subset = NULL,
    mvnTest = "hz")

# Testing multivariate normality by species
mvn(iris[iris$Species == "setosa", 1:4],
    subset = NULL,
    mvnTest = "mardia")

# Testing QQ Plot by species
mvn(iris[iris$Species == "setosa", 1:4],
    subset = NULL,
    mvnTest = "mardia", multivariatePlot = "qq")

#################### Comparison of normal and different t-distribution ####################
# Degrees of freedom
df = c(1,4,10,30,80)
colour = c("red", "darkorange2", "forestgreen", "goldenrod3","blueviolet","black")

# Generate a vector of 100 values between -6 and 6
x <- seq(-6, 6, length = 100)

# Plot a normal distribution
plot(x, dnorm(x), type = "l", lty = 2, xlab = "t-value", ylab = "Density", 
     main = "Comparison of t-distributions", col = "black")

# Add the t-distributions to the plot
for (i in 1:5){
  lines(x, dt(x, df[i]), col = colour[i])
}

# Add a legend
legend("topright", c("df = 1", "df = 4", "df = 10", "df = 30","df = 80", "normal"), 
       col = colour, title = "t-distributions", lty = c(1,1,1,1,1,2))


########################## Comparison of normal and t-distribution #############################

# Generate a vector of 100 values between -3 and 3
x <- seq(-3, 3, length = 100)

# Plot a normal and t distribution
plot(x, dnorm(x), type = "l", lty = 2, xlab = "t-value", ylab = "Density", 
     main = "Comparison of normal and t-distribution", col = "black") 
lines(x, dt(x, 1), col ="red")

legend("topright", c("t with  df 1", "normal"), 
       col = c("red", "black"), lty = c(1,2))

######################### Contour plots ###################################

######################## normal distribution ################################

library(mvtnorm) 
x.points <- seq(-2,6,length.out=100) 
y.points <- x.points 
z <- matrix(0,nrow=100,ncol=100) 
mu <- c(1,2) 
sigma <- matrix(c(1,0.5,0.5,2),nrow=2) 
for (i in 1:100) { 
  for (j in 1:100) { 
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]), mean=mu,sigma=sigma)
  } 
} 
contour(x.points,y.points,z)

######################## t-distribution ####################################

x.points <- seq(-2,6,length.out=100) 
y.points <- seq(-3,6,length.out=100) 
z <- matrix(0,nrow=100,ncol=100) 
mu <- c(1,2) 
sigma <- matrix(c(1,0.5,0.5,2),nrow=2) 
for (i in 1:100) { 
  for (j in 1:100) { 
    z[i,j] <- dmvt(c(x.points[i],y.points[j]), delta = mu,sigma=sigma, df = 1)
  } 
} 
contour(x.points,y.points,z)


############################## Generating random samples ################################
delta <- c(1, 2, -5)
sigma <- matrix(c(1, 1, 0,
                  1, 2, 0,
                  0, 0, 5), 3, 3)

# Generate samples
t.sample <- rmvt(n = 2000, delta = delta, sigma = sigma, df = 4)
head(t.sample,4)
library ("corrplot")
corrplot (cor (t.sample), method = "ellipse")

# t-distribution with df 10
t.sample <- rmvt(n = 2000, delta = delta, sigma = sigma, df = 10)
head(t.sample,4)
library ("corrplot")
corrplot (cor (t.sample), method = "ellipse")


############## Plotting the density of multivariate t-distribution ##############

x <- seq(-3, 6, by = 1); y <- seq(-3, 6, by = 1)
d <- expand.grid(x = x, y = x)
del1 <- c(1, 2); sig1 <- matrix(c(1, .5, .5, 2), 2)
dens <- dmvt(as.matrix(d), delta = del1, sigma = sig1, df = 10, log = FALSE)
library(scatterplot3d)
scatterplot3d(cbind(d, dens), type = "h", zlab = "density")

# Effect of changing df to 30
dens <- dmvt(as.matrix(d), delta = del1, sigma = sig1, df = 30, log = FALSE)
library(scatterplot3d)
scatterplot3d(cbind(d, dens), type = "h", zlab = "density")

###################### Cumulative density using pmvt ###########################
pmvt (lower = c(-1,-2), upper = c(2, 2), delta = c(1, 2), sigma = diag(2), df = 6)

###################### Inverse cdf of t-distribution ###########################
qmvt ( p = 0.95, sigma = diag (2), tail = "both", df = 3)


######################## Contour skew normal distribution ########################
library(sn)
x.points <- seq(-2,6,length.out=100) 
y.points <- seq(-2,6,length.out=100) 
z <- matrix(0,nrow=100,ncol=100) 
xi <- c(1,2) 
sigma <- matrix(c(1,0.5,0.5,2),nrow=2) 
alp <- c(-3, 3)
for (i in 1:100) { 
  for (j in 1:100) { 
    z[i,j] <- dmsn(c(x.points[i],y.points[j]), xi = xi, Omega = sigma, alpha = alp)
  } 
} 
contour(x.points,y.points,z)

###################### Generating skew-normal samples ##########################
xi1 <- c(1, 2,-5)
Omega1 <- matrix(c(1, 1, 0,
                   1, 2, 0,
                   0, 0, 5), 3, 3)
alpha1 <- c(4, 30,-5)
skew.sample <- rmsn(n = 2000, xi = xi1, Omega = Omega1, alpha = alpha1)
library ("corrplot")
corrplot (cor (skew.sample), method = "ellipse")

###################### Generating skew-t samples ##########################
xi1 <- c(1, 2,-5)
Omega1 <- matrix(c(1, 1, 0,
                   1, 2, 0,
                   0, 0, 5), 3, 3)
alpha1 <- c(4, 30,-5)
skewt.sample <- rmst(n = 2000, xi = xi1, Omega = Omega1, alpha = alpha1, nu = 4)
library ("corrplot")
corrplot (cor (skewt.sample), method = "ellipse")

###################### Estimating the parameters #########################

msn.mle (y = skew.sample, opt.method = "BFGS")

############################################################################################################################