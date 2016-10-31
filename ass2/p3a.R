lambda <- 2;
t0 <- 5;
t1 <- 20;
#t2 <- 40;
upper <- 100;
n.rep <- 1e5

pp <- numeric(n.rep);
pp1 <- numeric(n.rep);
#pp2 <- numeric(n.rep);

for (i in 1:n.rep) {
    N <-  rpois(1,lambda * upper);
    Un <-  runif(N, 0, upper);
    Sn <-  sort(Un);
    n <-  min(which(Sn > t0));
    n1 <-  min(which(Sn > t1));
 #  n2 <-  min(which(Sn > t2));
    
    pp[i] <- n - 1; #
    pp1[i] <- n1 - 1;
 #   pp2[i] <- n2 - 1;
}

N5 <- c(mean(pp),sd(pp));N5
N20 <- c(mean(pp1),sd(pp1));N20
#N40 <- c(mean(pp2),sd(pp2));N40

## c)

lambda <- 2
t0 <- 20;

# 30 instances
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
#EN40.hat <- N40[1];

# dra points of expected value
points(x=5,y=EN5.hat, col="red", bg="red", pch=16);

points(x=20,y=EN20.hat ,col="blue", bg="blue", pch=16)

#points(x=40,y=EN40.hat ,col="green", bg="green", pch=16)

abline(v=5,h=EN5.hat)
abline(v=20,h=EN20.hat);

SD5.hat <- N5[2]
SD20.hat <- N20[2]
#SD40.hat <- N40[2]

# sd points:
points(x=5,y=(N5[1]-SD5.hat), col="yellow", bg="yellow", pch=16);
points(x=5,y=(N5[1]+SD5.hat), col="yellow", bg="yellow", pch=16);

points(x=20,y=(N20[1]-SD20.hat), col="yellow", bg="yellow", pch=16);
points(x=20,y=(N20[1]+SD20.hat), col="yellow", bg="yellow", pch=16);

#points(x=40,y=(N40[1]-SD40.hat), col="yellow", bg="yellow", pch=16);
#points(x=40,y=(N40[1]+SD40.hat), col="yellow", bg="yellow", pch=16);

