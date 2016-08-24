x <- runif(1000,0,1);

hist(x);

sum(x > 0.5)

plot(sin(x))


myfunc <- function (a,b,c){
  r <- 0;
  if (((-b^2)+4*a*c) < 0) {
    return ("complex solution");
  }
  
  x <- (-b)+sqrt((-b^2)+4*a*c)/2*a;
  y <- (-b)-sqrt((-b^2)+4*a*c)/2*a;
  
  return(c(x,y));
}

print(myfunc(2,2,55))

#5

pop <-  1.0+norm(10000);

conf95 <- function(pops) {
  n <- length(pops);
  pop.mean = mean(pops);
  s <- sd(pops);
  tmp <- (pop.mean + 1.96 * sqrt(s^2/n))
  x <- floor(tmp)
  tmp <- (pop.mean - 1.96 * sqrt(s^2/n))
  y <- floor(tmp)
  
  return(c(x,y))
  
}

print(conf95(pop));
