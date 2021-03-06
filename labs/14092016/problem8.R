### Mark of Chain
### Markov chain
rm (list=ls())
# the rows of the tpm:

p.0j <- c(0.9,0.07,0.02,0.01)
p.1j <- c(0.0,0.95,0.04,0.01)
p.2j <- c(0,0,0.9,0.1)
p.3j <- c(0,0,0,1.0)

# Arranged in matrix, the tpm, P:
P <- matrix( data=c(p.0j, p.1j, p.2j, p.3j),
             ncol=4, nrow=4,
             byrow=TRUE); P

states <- c(0,1,2,3)
n.max <- 200

x <- numeric(n.max)
n <- 1
x[n] <- 0 # start in state 0 at time n=1
for ( n in 2:n.max) # while state is lower than 3
{
  x[n] <- sample( x=states, size=1,
                  prob=P[ x[n-1] + 1,])
}
# cbind(x)
plot( x=1:n.max, y=x, type='s')


####---- 

Markov.realization.state.3 <- function() {
  p.0j <- c(0.9,0.07,0.02,0.01)
  p.1j <- c(0.0,0.95,0.04,0.01)
  p.2j <- c(0,0,0.9,0.1)
  p.3j <- c(0,0,0,1.0)
  # Arranged in a matrix, the tpm, P:
  P <- matrix( c(p.0j,p.1j,p.2j,p.3j), ncol=4, nrow=4,
               byrow=TRUE)
  
  
  states <- c(0,1,2,3)
  n.max <- 200
  x <- numeric(n.max)
  n <- 1
  x[n] <- 0 # start in state 0 at time n=1
  while ( x[n]<3 & n<n.max) # while state is lower than 3
  {
    x[n+1] <- sample( x=states, size=1,
                    prob=P[ x[n] + 1,]) # state i <-> 
    n <- n+1
  }
  return(n)
}


Markov.realization.state.3()

n.sim <-10^4
T <- numeric(n.sim)
for( i in 1:n.sim) {
  T[i] <- Markov.realization.state.3()
}

hist(T, dens=20); max(T)
s <- pmin(10, T) # cbind( T, pmin(10, T))
hist(s, dens=20)
mean( s ) # Estimate of E(S)
mean( s==8 ) # Estimate of P(S)

###
mean( T==8); mean(T)
###
