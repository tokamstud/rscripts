rm( list=ls() ) # Clears all
#p <- c(rep(0.9,15));
p <- c( .9, .9, .7, .8, .7, .9, .7, .7, .8, .8, .7, .9, .8, .9, .8 )
n <- 1e6
u.mtx <- matrix( runif( n=n*15 ), ncol=15 )
f.matrix <- ( u.mtx < matrix( rep( p, times=n), byrow=T, ncol=15 ) )
y.f <- rowSums(f.mtx)
sum( y.f >= 11 )/n.replic
sd(y.f >= 11) / sqrt(n.replic);
