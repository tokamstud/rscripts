die.toss <- sample(x=1:6, size=5, replace=TRUE, prob=rep(1/6,6));
sum(die.toss);

n.sim <- 10^4;
s.array <- rep(NA, n.sim);

for (i in 1:n.sim ) {
  die.toss <- sample(x=1:6, size=5, replace=TRUE, prob=rep(1/6,6))
  s.array[i] <- sum(die.toss)
#  print( c(i,die.toss))
}

s.array >= 25;

mean(s.array >= 25);

mean(s.array < 10);

hist(s.array, prob=T);
hist(s.array,prob=T, breaks=seq(4.5,30,1));






