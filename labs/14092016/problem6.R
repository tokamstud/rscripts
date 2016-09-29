rm ( list-ls() )

n.replic <- 1000
a <- 2
b <- 10
u.1 <- runif( n=n.replic, min=a/2, max=b/2)
u.2 <-runif( n=n.replic, min=a/2, max=b/2)
y=u.1+u.2

hist ( y, prob=T, density=10) 

v<- seq(2,6,.01)
lines( x=v, y= 2*(v-2)/32, col=2,lwd=2)

v <-seq(6,10,.01)
lines( x=v, y= -2*(v-10)/32, col=2, lwd=2)


###--- Estimate of P(resource >= 8 | finding)
sum(y >= 8)/n.replic
mean(y >= 8)


p.finding <- 0.4
x <- sample(x=c(0,y), size= n.replic, replace=TRUE,
            prob=c( 1-p.finding, p.finding*rep(1/n.replic, n.replic) ))


hist(x, prob=T, density=10)


###--- Alternative
find <- sample(x=c(0,1), size=n.replic, replace=TRUE,
               prob=c(0.6,0.4) )

head(find)
mean(find)

x.new <- y*find
hist( x.new, prob=T, density=10)

###
sum( x >= 8 )
mean( x.new >= 8)