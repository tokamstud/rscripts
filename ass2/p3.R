n <- 1e5
lambda = 2
t.max = 20
T <- rexp(n=8*t.max, lambda)
S.t <- cumsum(T)
Nt <- 0:length(S.t)
plot(x=c(0,S.t),y=Nt, type='s')

mean(T);sd(T)
n <- min(which(S.t>5))-1
n <- min(which(S.t>20))-1

