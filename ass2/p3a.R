lambda <- 2;
t0 <- 20;
t1 <- 5;
t2 <- 10;
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
    n2 <-  min(which(Sn > t2));
    
    pp[i] <- n - 1;
    pp1[i] <- n1 - 1;
}

c(mean(pp),sd(pp));
c(mean(pp1),sd(pp1));

## c)

lambda <- 2
t0 <- 20;

for (i in 1:30) {
    Tn <-  rexp(100, lambda);
    Sn <- cumsum(Tn);

    n <-  min(which(Sn > t0));
    if (i == 1) {
        plot(Sn);
    }   
    points(Sn);
}
