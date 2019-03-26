## Simulation Study: 
library(dplyr);library(tibble);library(ggplot2)
#Paul Harmon

#install.packages('actuar')
library(actuar)


#simulate the data using the actuar package's data
m <- 100
p <- .8
sim.dat <- rztbinom(m, size = 2, prob = p)
#sim.dat <- rbinom(100, m, p)
#True Mean
TM <- m*p/(1-(1-p)^m)

#bootstrap_function: 
boot_bin <- function(n.boot, data){
  #initializes a matrix to store bootstrapped values
  boot_mean <- rep(0, n.boot)
  for (j in 1:n.boot){
    #samples indices from observed data (a vector)
    bs.index <- sample(1:length(data),n.boot,replace = TRUE)
    bs <- data[bs.index]
    #calculates the estimate we need
    boot_mean[j] <- mean(bs)/m
  }
  
return(boot_mean)}

#sample mean
BIN.mle <- mean(sim.dat)/m #gives us the sum of the x_i divided by the number of reps, i.e. xbar
#create a bootstrapped estimate of the 95% CI
BIN.sd.boot <- var(boot_bin(500, data = sim.dat))
BIN.alpha <- rgb(200,30,120,alpha = 80, maxColorValue = 255)
#mean(sim.dat) gives the same output

#zero-truncated binomial estimator (still iffy on this one!)
x <- sim.dat
ZBIN.mle <- (-(2*sum(x) +4*m - 4) + sqrt((-2*sum(x)-4*m + 4)^2 - 4*(-2*sum(x) - 4*m + 4)*(2*sum(x) + 2)))/(4*(sum(x) + m - 1))

boot_Zbin <- function(n.boot, data){
  #initializes a matrix to store bootstrapped values
  boot_mean <- rep(0, n.boot)
  for (j in 1:n.boot){
    #samples indices from observed data (a vector)
    bs.index <- sample(1:length(data),n.boot,replace = TRUE)
    x <- data[bs.index]
    #calculates the estimate we need
    boot_mean[j] <-  (-(2*sum(x) +4*m - 4) + sqrt((-2*sum(x)-4*m + 4)^2 - 4*(-2*sum(x) - 4*m + 4)*(2*sum(x) + 2)))/(4*(sum(x) + m - 1))

  }
  
  return(boot_mean)}
ZBIN.var <- var(boot_Zbin(500, data = sim.dat)) #var of bootstrapped version
Z.alpha <- rgb(60,200,100,alpha = 80, maxColorValue = 255)
  
## How quickly could I put together a shiny app for this? 
#     sliders for m, p
#     show the two estimators

plot(0:2, 0:2, type = "n", main = "Truncated vs. Binomial Simulated Means")
abline(v = p, col = "blue", lwd = 2)
abline(v = ZBIN.mle, col = rgb(60,200,100, maxColorValue = 255)) #adds Zero-Truncated Version
  rect(xleft = ZBIN.mle-ZBIN.var, xright = ZBIN.mle + ZBIN.var,ybottom = .5, ytop = 1, col = Z.alpha)
abline(v = BIN.mle, col = rgb(200,30,120, maxColorValue = 255)) #adds MLE in 
rect(xleft = BIN.mle-BIN.sd.boot, xright = BIN.mle + BIN.sd.boot,ybottom = 0, ytop = .5, col = BIN.alpha)
legend('topleft', fill = c(rgb(60,200,100, maxColorValue = 255),rgb(200,30,120, maxColorValue = 255),'blue'), legend = c('Truncated','Binomial','True Prob'), bty = 'n')  
  

#try this
hist()
  
  