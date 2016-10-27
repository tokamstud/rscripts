n = 1e3

EX1 <- 0; sd1 <- 1;
EX2 <- 0; sd2 <- 1;

# lazy function
makecor <- function(rho12) {
    d <- 2 #dimention

    Rho <- matrix (c(1,rho12,rho12,1),ncol=2);Rho

    Sigma <-  Rho * c(sd1,sd2) %*% t(c(sd1,sd2)); Sigma

    A <- chol(Sigma);round(A,3)

    Z <-  matrix(rnorm(n*d), nrow=n,ncol=d)
    X <- Z %*% A + matrix( c(EX1,EX2),nrow=n,ncol=d,byrow=TRUE)

    pairs(X,labels=expression(X[1],X[2]))

    U1 <- pnorm(X[,1])
    U2 <- pnorm(X[,2])

    T1 <- -log(U1)
    T2 <- -log(U2)
    plot(T1,T2)
    plot(U1,U2)
}

makecor(-0.9)
makecor(0.9)

