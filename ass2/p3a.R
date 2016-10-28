lambda <- 2;
t0 <- 5;
t1 <- 20;
upper <- 100;
n <- 1e5

pp <- numeric(n);
pp1 <- numeric(n);

for (i in 1:n) {
    N <-  rpois(1,lambda * upper);
    Un <-  runif(N, 0, upper);
    Sn <-  sort(Un);
    n <-  min(which(Sn > t0));
    n1 <-  min(which(Sn > t1));
    
    pp[i] <- n - 1;
    pp1[i] <- n1 - 1;
}

N5 <- c(mean(pp),sd(pp));N5
N20 <- c(mean(pp1),sd(pp1));N20

## c)

lambda <- 2
t0 <- 20;

for (i in 1:30) {
    Tn <-  rexp(100, lambda);
    Sn <- cumsum(Tn);
    n <-  min(which(Sn > t0));
    Nt <- 0:length(Sn)
    if (i == 1) {
        plot(x=c(0,Sn),y=Nt,type='s')
    }   
    points(x=c(0,Sn),y=Nt, type='s')
}
EN5.hat <- N5[1];
EN20.hat <- N20[1];

points(x=5,y=EN5.hat, col="red", bg="red", pch=16);

points(x=20,y=EN20.hat ,col="blue", bg="blue", pch=16)

abline(v=5,h=EN5.hat)
abline(v=20,h=EN20.hat);



#points(x=5,y=N5[2],col="red");
#points(x=10,y=N10[2],col="blue");

