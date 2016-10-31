dopois <- function() {
    
    lambda <- 2.25;
    
    t0 <- 20
    
    Tn <- rexp(1000, lambda)

    Sn <-  cumsum(Tn)

    n <-  min(which(Sn>t0))
    ret = c(n,Sn)
    return (ret)
}
n <- dopois()[1];
n-1

pp <-  numeric(1e4)
for (i in c(1:1e4)) {
    pp[i] <- dopois()[1];
}

mean(pp-1)

for (i in 1:30) {
    Sn <- dopois()[2];
    Nt <- 0:length(Sn)
    if (i == 1) {
        plot(x=c(0,Sn),y=Nt,type='s')
    }   
    points(x=c(0,Sn),y=Nt, type='s')
}

lambda <- 2
t0 <- 20;
for (i in 1:30) {
    Tn <-  rexp(100, lambda);
    Sn <- cumsum(Tn);
    Nt <- 0:length(Sn)
    if (i == 1) {
        plot(x=c(0,Sn),y=Nt,type='s')
    }   
    points(x=c(0,Sn),y=Nt, type='s')
}


P <- rpois(1000,2.25)
print("....")
mean(P)
var(P)
sqrt(var(P))
sd(P)
