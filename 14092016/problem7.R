rm (list=ls())

n.replic <- 10^5
y <- numeric( n.replic) 
k <- 0 # Number of accepted simulated values
  while (k<n.replic) {
    u <- runif(1)
    x <- runif(1)
    if (16*(x^2)*((1-x)^2) > u) # Accept if
    {
      k <- k+1
      y[k] <- x
    }
  }

hist(y, prob=T, density=10)
x<- seq(0,1,0.01)
lines(x=x, y=30*(x^2)*((1-x)^2), col=2, lwd=2)

###--- Simulation estimate of P(0.6 <= X <= 0.9)
mean(y>= 0.6 & y<=0.9)  # Estimate of 


###--- true value P(0.6 <= X <= 0.9)
30*(((.9^3)/3-(.9^4)/2+(.9^5)/5)- ((.6^3)/3-(.6^4)/2+(.6^5)/5))
