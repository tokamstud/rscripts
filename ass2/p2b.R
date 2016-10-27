rm( list=ls() ) # Clears all
n = 10 ^ 6
t <- seq(0,1,.01)
plot( t, sin(t), type='l', col=4 )
abline( h=0 ); abline( v=1 )
u <- runif( n = n, 0, 1 )
mean( 4*sqrt(1-u^2) )*1
(sd( 4*sqrt(1-u^2) )*1)/sqrt(n )
