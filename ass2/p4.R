#b)

n = 1e4

EX1 <- 0; sd1 <- 1;
EX2 <- 0; sd2 <- 1;

# lazy function
makecor <- function(rho12) {
    d <- 2 #dimention

    Rho <- matrix (c(1,rho12,rho12,1),ncol=2)

    Sigma <-  Rho * c(sd1,sd2) %*% t(c(sd1,sd2))

    A <- chol(Sigma);round(A,3)

    Z <-  matrix(rnorm(n*d), nrow=n,ncol=d)
    X <- Z %*% A + matrix( c(EX1,EX2),nrow=n,ncol=d,byrow=TRUE)

    pairs(X,labels=expression(X[1],X[2]),cex=0.1)

    U1 <- pnorm(X[,1])
    U2 <- pnorm(X[,2])

    T1 <- -log(U1)
    T2 <- -log(U2)
    plot(T1,T2,cex=0.1)
    plot(U1,U2,cex=0.1)
#c)
    Y <- T1+T2
    return (Y)
}

Y1 <- makecor(-0.9)
Y2 <- makecor(0.9)

#c)
hist(Y1, prob=TRUE,freq=FALSE, dens=20)
curve(dnorm(x, mean=mean(Y1), sd=sd(Y1)),add=TRUE, col="blue", from=4, lwd=3)
text(6,0.1,round(mean(Y1 >= 4),3),col="blue");

hist(Y2, prob=TRUE,freq=FALSE, dens=20)
curve(dnorm(x, mean=mean(Y2), sd=sd(Y2)),add=TRUE, col="blue", from=4, lwd=3)
text(8,0.1,round(mean(Y2 >= 4),3),col="blue");

                                        #d)
print("::::")
round(mean(Y1>=4),3);round(sd(Y1>=4),3)/sqrt(n);
round(mean(Y2>=4),3);round(sd(Y2>=4),3)/sqrt(n);
