## Simulation Study: 
library(dplyr);library(tibble);library(ggplot2)
#Paul Harmon

install.packages('actuar')
library(actuar)


#simulate the data using the actuar package's data
m <- 2
p <- .8
sim.dat <- rztbinom(100, size = m, prob = p)
#sim.dat <- rbinom(100, m, p)
#True Mean
TM <- m*p/(1-(1-p)^m)


#sample mean
BIN.mle <- mean(sim.dat)/m #gives us the sum of the x_i divided by the number of reps, i.e. xbar
BIN.wald.sd <- sqrt(BIN.mle *(1-BIN.mle)/2) #gives the wald CI
BIN.alpha <- rgb(200,30,120,alpha = 80, maxColorValue = 255)
#mean(sim.dat) gives the same output

#binomial estimator
ZBIN.mle <- mean(sim.dat)/2 #sample output
ZBIN.var <- sqrt(ZBIN.mle *(1-ZBIN.mle)/20) #test output
Z.alpha <- rgb(60,200,100,alpha = 80, maxColorValue = 255)
  
## How quickly could I put together a shiny app for this? 
#     sliders for m, p
#     show the two estimators

plot(0:2, 0:2, type = "n", main = "Truncated vs. Binomial Simulated Means")
abline(v = p, col = "blue", lwd = 2)
abline(v = ZBIN.mle, col = rgb(60,200,100, maxColorValue = 255)) #adds Zero-Truncated Version
  rect(xleft = ZBIN.mle-ZBIN.var, xright = ZBIN.mle + ZBIN.var,ybottom = .5, ytop = 1, col = Z.alpha)
abline(v = BIN.mle, col = rgb(200,30,120, maxColorValue = 255)) #adds MLE in 
rect(xleft = BIN.mle-BIN.wald.sd, xright = BIN.mle + BIN.wald.sd,ybottom = 0, ytop = .5, col = BIN.alpha)
legend('topleft', fill = c(rgb(60,200,100, maxColorValue = 255),rgb(200,30,120, maxColorValue = 255),'blue'), legend = c('Truncated','Binomial','True Prob'), bty = 'n')  
  
  
  
  