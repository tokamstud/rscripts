a <- 5;
b = 7;
a*b;
a/b;


c <- seq(from=-2, to=5, by=0.1);

x <- seq(0,3,0.5);  x[3]; x[c(3,4,5)];
x[3:5];

m <- matrix(1:9,nrow=3,ncol=3);
m <- matrix(1:9,nrow=3,ncol=3, byrow=TRUE);
m[,2];
m[3,2];
m[2];
m[1:9];
m[4:5];
#bigshit <- matrix(1:9e10);

m.sub <- m[1,c(1,2)];
m.sub.2 <- m[2:3,c(1,2)];

x;
y <- x; # makes y a vector/array of same length as x
for ( i in 1:7 ) y <- x[i]+2;
y;
y <- x+2;  w <- x/5; y/x;

mean(x); sum(x); mean(y/x);

x <- seq(-3,3, 0.05);
plot(x=x, y=-3-x^2,type="1",lwd=2,col='red');


x <- 1:6;
p <- rep(1/6,6);
E.x <- sum(x*p);

E.x.sq <- sum(x^2*p); E.x.sq

(V.x <- E.x.sq-E.x^2);
(SD <- sqrt(5*V.x));
