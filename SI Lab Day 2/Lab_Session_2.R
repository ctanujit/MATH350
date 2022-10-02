################# Density plot of normal distribution with mean 2 and variance 1 ########################

# Create a sequence of numbers between -2 and 6 incrementing by 0.01.
x <- seq(-2, 6, by = .01)
# Choose the mean as 2 and standard deviation as 1.
y <- dnorm(x, mean = 2, sd = 1)
plot(x,y,col = "mediumvioletred", main = "Normal distribution with mean 2 and variance 1" )

############################## Multivariate Normal ###############################
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


# Density at multiple points using dmvnorm functios
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

########################################################################################################################