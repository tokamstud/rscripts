rm( list=ls() ) # Clears all
p <- c( .9, .9, .7, .8, .7, .9, .7, .7, .8, .8, .7, .9, .8, .9, .8 )
p <- c(rep(0.9,15));
###--- iv)
n.replic <- 1e6
###--- Matrix of 500 x 10 U[0,1]-values:
u.matrix <- matrix( runif( n=n.replic*15 ), ncol=15 )
functioning.matrix <- ( u.matrix < matrix( rep( p, times=n.replic), byrow=T, ncol=15 ) )
#print(matrix(rep(p,times=n.replic), byrow=T, ncol=15));
#print(u.matrix);
#print(functioning.matrix);
y.functioning <- rowSums(functioning.matrix)
#print(y.functioning);
hist( y.functioning, prob=TRUE, density=10 )
###--- Estimate of P( at eleven seven work ):
sum( y.functioning >= 11 )/n.replic

sd(y.functioning >= 11) / sqrt(n.replic);
