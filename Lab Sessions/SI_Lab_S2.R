
############# Exploring Iris Data set ###########

# Read and view first 4 observations of Iris data
data(iris)
head(iris,n = 4)

# What is the dimension of the data
dim(iris)

# Extract the column names
names(iris)

# Access Sepal length and Sepal width columns of observations 8 to 10
iris [8:10, 1:2]

# Check the data types
str(iris)

# Reassign factor labels
library (car)
iris$Species <- recode (iris$Species, "'setosa' = 1; 'versicolor' =  2; 'virginica' = 3")
# Verify the data types
str(iris)

# Calculate the mean
colMeans(iris[, 1:4])

# Calculate the group mean using by
by(data = iris[,1:4], INDICES = iris$Species, FUN = colMeans)

# Calculate the group mean using aggregate
aggregate(.~ Species, iris, mean)

# Calculate the variance-covariance and correlation matrices
var(iris[, 1:4])
cor(iris[, 1:4])

# Visualize correlation matrix
library(corrplot)
corrplot(cor(iris[, 1:4]), method = "ellipse")

# Visualization of multivarate data

# Basic R plot
pairs(iris[, 1:4])

# Pairs plot by color
pairs(iris[, 1:4], col = iris$Species)

# Lattice
library(lattice)
splom(~iris[, 1:4], col = iris$Species, pch = 16)

# ggplot for visualization
library(ggplot2)

# Plot petal length vs petal width for different species
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(aes(color = Species, shape = Species))

# Add a linear trend line to the plot
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(method = lm)

# Plot the boxplot of Sepal length grouped by species
ggplot(data = iris) +
  aes(x = Species, y = Sepal.Length, color = Species) +
  geom_boxplot()

# Add the corresponding measurements to the plot
ggplot(data = iris) +
  aes(x = Species, y = Sepal.Length, color = Species) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

# Visualize the density plot of Petal length of different species
ggplot(data = iris) +
  aes(x = Petal.Length, fill = Species) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Species, nrow = 3)

# Correlation plot
library(GGally)
ggpairs(data = iris, columns = 1:4, mapping = aes(color = Species))

# 3D Plot
library(scatterplot3d)
scatterplot3d(iris[, c(1, 3, 4)], color = as.numeric(iris$Species))

# change the angle between x-axis and y-axis to 80 degrees
scatterplot3d(iris[, c(1, 3, 4)], color = as.numeric(iris$Species),pch = 4, angle = 80)

################# Density plot of normal distribution with mean 2 and variance 1 ########################

# Create a sequence of numbers between -2 and 6 incrementing by 0.01.
x <- seq(-2, 6, by = .01)
# Choose the mean as 2 and standard deviation as 1.
y <- dnorm(x, mean = 2, sd = 1)
plot(x,y,col = "mediumvioletred", main = "Normal distribution with mean 2 and variance 1" )


########### Skew-Normal Distribution #############
rm(list=ls())
# Required packages
library(sn)
library(knitr)

# Simulated data
set.seed(1234)
data.sim <- rsn(500,0,1,5)

hist(data.sim, breaks = 20, col = "blue")
box()

######################## END #######################
