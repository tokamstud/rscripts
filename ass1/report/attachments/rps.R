### Rock Paper Scissors
# Author: Tomsaz Gliniecki
## how many trials to win/loose
## how many trials to win (win = 3 wins)

# R=1,S=3,P=2;
elements <- c("R","P","S");
p <- c(1/3,1/3,1/3);
n <- 1e5;

ties <- 0;

play_rps <- function(lives=1) {
    p0win <- 0;
    p1win <- 0;
    #game loop
    t <- 0;
    while(1) {

        #independent choices
        player0 <- sample(elements,size=1,prob=p);
        player1 <- sample(elements,size=1,prob=p);

        ifelse ((player0=="R" && player1=="S"), p0win <- p0win+1,
        ifelse ((player0=="R" && player1=="P"), p1win <- p1win+1,
        
        ifelse ((player0=="P" && player1=="R"), p0win <- p0win+1,
        ifelse ((player0=="P" && player1=="S"), p1win <- p1win+1,

        ifelse ((player0=="S" && player1=="P"), p0win <- p0win+1,
        ifelse ((player0=="S" && player1=="R"), p1win <- p1win+1,NA))))));

        t <- t+1;       

        if (p0win>=lives || p1win>=lives) {
            break
        }
    }

    return(t);
}
# arrays of numbers of trials till succes per simulation.
k.1 = c();
k.3 = c();
k.5 = c();

# simulation loop 1 lives
for (i in c(0:n)) {
    k.1<- c(k.1,play_rps());
}
    # draw graph
hist(k.1,dens=20,breaks=seq(from=(min(k.1)-0.5),to=(max(k.1)+0.5),by=1),freq=FALSE);mean(k.1);
abline(v=mean(k.1),col="red",lwd=2);
text(3,0.5,"estimate of E(X)", col = "red");
text(3,0.6,round(mean(k.1),digits=3), col = "red");

curve(dnorm(x+0.5, mean=mean(k.1), sd=sd(k.1)),to=5, add=TRUE, col="blue", lwd=2);
text(4,0.2,round(mean(k.1 <= 5),3),col="blue");
text(4,0.3,"P(X<=5)",col="blue");

# simulation loop 3 lives
for (i in c(0:n)) {
    k.3 <- c(k.3,play_rps(3));
}
    # draw graph
hist(k.3,dens=20,breaks=seq(from=(min(k.3)-0.5),to=(max(k.3)+0.5),by=1),freq=FALSE);mean(k.3);
abline(v=mean(k.3),col="red",lwd=2);
text(9,0.15,"estimate of E(X)", col = "red");
text(9,0.14,round(mean(k.3),digits=3), col = "red");

curve(dnorm(x+0.5, mean=mean(k.3), sd=sd(k.3)),from=8, add=TRUE, col="blue", lwd=2);
text(12,0.05,round(mean(k.3 >= 8),3),col="blue");
text(12,0.06,"P(X>=8)",col="blue");

# simulation loop 5 lives
for (i in c(0:n)) {
    k.5 <- c(k.5,play_rps(5));
}
    # draw graph
hist(k.5,dens=20,breaks=seq(from=(min(k.5)-0.5),to=(max(k.5)+0.5),by=1),freq=FALSE);mean(k.5);
abline(v=mean(k.5),col="red",lwd=2);
text(15,0.12,"estimate of E(X)", col = "red");
text(15,0.11,round(mean(k.5),digits=3), col = "red");

curve(dnorm(x+0.5, mean=mean(k.5), sd=sd(k.5)),from=8,add=TRUE, col="blue", lwd=2);
text(20,0.04,round(mean(k.5 >= 8),3),col="blue");
text(20,0.05,"P(X>=8)",col="blue");
