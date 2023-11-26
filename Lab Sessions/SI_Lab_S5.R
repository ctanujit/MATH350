

############## Example 1 : Estimating Pi ###############

num_darts <- 1000
num_darts_in_circle <- 0 # We have not thrown any darts now
x <- runif(n=1,min=-1,max=1) # random number between -1 and 1
y <- runif(n=1,min=-1,max=1)
?runif # R Help

# We need Thousand Throws
for(i in 1: num_darts){
  x <- runif(n=1,min=-1,max=1) 
  y <- runif(n=1,min=-1,max=1)
  # 1000 values are assigned
  if(x^2+y^2 <= 1){ # condition for the dart inside the circle
    num_darts_in_circle <- num_darts_in_circle + 1 # counting that one dart inside the circle
  }
}

print(4 * num_darts_in_circle / num_darts)
# Values will change in every time we run it. Can be stable if we set num_dart very high
# But this will increase the running time. So we try to optimize the code.

### Fast Implementation and Optimized Code ###
num_darts <- 100000
num_darts_in_circle <- 0 # We have not thrown any darts now
x <- runif(n=num_darts, min=-1, max=1) 
y <- runif(n=num_darts, min=-1, max=1)
sum_square <- x^2 + y^2 
# x, y, sum_square are vectors of length 100000
indexes_darts_in_circle <- which(sum_square <= 1) # counter
num_darts_in_circle <- length(indexes_darts_in_circle) # check ?length and ?which
print(4 * num_darts_in_circle / num_darts)

### Visualization darts ###
plot(x,y)


############## Example 2 : Monty Hall Problem ############

set.seed (1) # To obtain reproducible results
repeats <- 1e4 # We will repeat the experiment 10000 times
win.no.switch <- numeric(length = repeats) # will save 0 / 1 based on winning no switch
win.switch <- numeric(length = repeats)   # will save 0 / 1 based on winning switch

for(r in 1:repeats) # Repeat process many times
{
  # The setup
  doors <- 1:3    # three doors
  prize <- sample(1:3, 1)  # randomly select the door which has the prize
  
  # Contestants are ready. Game starts
  chosen.door <- sample(1:3, 1)   # choose a door
  
  # reveal a door that is not the chosen door and not the door with a prize in it
  which.reveal <- rep((1:3)[-c(prize, chosen.door)], 2) # doing rep(., 2) because sample() is being annoying
  reveal <- sample(which.reveal, size = 1) # randomly choose which door to reveal
  
  win.no.switch[r] <- chosen.door == prize  #tracking win if don't change door
  
  chosen.door <- (1:3)[-c(reveal, chosen.door)] #change door
  win.switch[r] <- chosen.door == prize # tracking win if change door
}

### Monty Hall Problem Outputs ###

head(win.no.switch) # first few results for no switching
head(win.switch) # first few results of switching

mean(win.no.switch) #Prob of winning if you don't switch
mean(win.switch) # Prob of winning if you switch


############### Example 3 : Monte Carlo Integration #############

### Single Integral Problem ###
f <- function(x)x^2
curve(f, from = -3, to = 3) # plot the function
install.packages("DescTools")
library(DescTools)
Shade(f, breaks = c(-3,3), col = "red") # add background grid to the plot
grid()
# Estimate the shaded region by Monte Carlo Simulation
# 2000 random samples are generated uniformly between (-3, 3)
query_points <- runif(n = 2000, min=-3, max=3)
area_shaded_region <- 6 * mean(f(query_points))

area_estimates <- vector(length=10000) # increase the random samples
for(i in 1: 10000) 
  {
  query_points <- runif(n=i, min =-3, max=3) 
  area_estimates[i]<- 6 * mean(f(query_points))
  }
  plot(area_estimates)
  abline(h=18, col = "red", lwd =2)

### Double Integral Problem ###
  
g <- function(x,y)x^2 + y^2
x <- y <- seq(-3, 3, length = 100) # generate 100 (x,y) values 
z <- outer(x, y, g)
persp(x, y, z)
install.packages("plotly")
library(plotly)
plot_ly(x = x, y = y, z = z) %>% add_surface()

# 2000 random samples are generated uniformly between (-3, 3)
query_points_x <- runif(n = 1000, min=-3, max=3)
query_points_y <- runif(n = 1000, min=-3, max=3)
volume_shaded_region <- 36 * mean(g(query_points_x, query_points_y)) # MC Simulations

volume_estimates <- vector(length = 10000) # increase sample points
for(i in 1: 10000) 
{
  query_points_x <- runif(n=i, min =-3, max=3) 
  query_points_y <- runif(n=i, min =-3, max=3) 
  volume_estimates[i]<- 36 * mean(g(query_points_x, query_points_y))
}
plot(volume_estimates)
abline(h=216, col = "red", lwd =2)


############# Assignment 1 : Integral Problem & Computing Pi ###################

h <- Vectorize(function(x,y){ # x and y can be vector of numbers (or only numbers)
  if(x^2 + y^2 <= 1){
    return(1)
  }
  0
}, vectorize.args = c("x","y"))

#### Cross-checking ###
# h(1,1)
# h(0,0)
# h(c(0,1),c(0,1)) # without using Vectorize this will not show up. 

x <- y <- seq(-1, 1, length = 400)
z <- outer(x, y, h)
plot_ly(x = x, y = y, z = z) %>% add_surface()

query_points_x <- runif(n = 2000, min=-1, max=1)
query_points_y <- runif(n = 2000, min=-1, max=1)
volume_or_estimated_pi <- 4 * mean(h(query_points_x, query_points_y)) # Close to 3.14

### Improving Accuracy ###
volume_or_estimated_pi <- vector(length = 10000) # increase sample points
for(i in 1: 10000) 
{
  query_points_x <- runif(n=i, min =-1, max=1) 
  query_points_y <- runif(n=i, min =-1, max=1) 
  volume_or_estimated_pi[i]<- 4 * mean(h(query_points_x, query_points_y))
}
plot(volume_or_estimated_pi)
abline(h=pi, col = "red", lwd =2)


################ Assignment 2 : Toy Collector Problem #########################

set.seed(1) # To obtain reproducible results

# the setup
prob.table <- c(.2, .1, .1, .1, .1, .1, .05, .05, .05, .05, .02, .02, .02, .02, .02) #Prob. distribution
boxes <- 1:length(prob.table) #1:15

# Create a function box.count to draw realizations of the RV of interest (x)
box.count <- function(prob)
{
  check <- rep(0, length(prob))
  i <- 0 # counter
  while(sum(check) < length(prob)) # check if all toys collected
  {
    x <- sample(boxes, 1, prob = prob) # generate a toy with given prob
    check[x] <- 1    # x has been collected
    i <- i + 1
  }
  return(i)
}

repeats <- 1e4  
sim.boxes <- numeric(repeats)
for(i in 1:repeats)
{
  sim.boxes[i] <- box.count(prob = prob.table)
}

hist(sim.boxes, breaks = 30) # Draw Histogram
mean(sim.boxes) # Average No. of chips packets needed to get all different toys 


####################### Example 6: Integral Problem #########################

set.seed(1)
repeats <- 1e4

esin <- numeric(length = repeats)
for(i in 1:repeats)
{
  samp <- runif(1, min = 0, max = pi) # draw from U(0, pi)
  esin[i] <- exp(sin(samp))
}
pi * mean(esin)  #pi*E(exp(sin(x))))

####################### END OF SESSION ###########################

