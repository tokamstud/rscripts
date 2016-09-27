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
hist(k.1,dens=20,breaks=max(k.1));mean(k.1);
abline(v=mean(k.1),col="red");
text(3,5e4,"estimate of E(X)", col = "red")
text(3,4e4,round(mean(k.1),digits=3), col = "red")

# simulation loop 3 lives
for (i in c(0:n)) {
    k.3 <- c(k.3,play_rps(3));
}
hist(k.3,dens=20,breaks=max(k.3));mean(k.3);
abline(v=mean(k.3),col="red");
text(8,1.7e4,"estimate of E(X)", col = "red")
text(8,1.6e4,round(mean(k.3),digits=3), col = "red")


# simulation loop 5 lives
for (i in c(0:n)) {
    k.5 <- c(k.5,play_rps(5));
}
hist(k.5,dens=20,max(k.5));mean(k.5);
abline(v=mean(k.5),col="red");
text(15,1.2e4,"estimate of E(X)", col = "red")
text(15,1.1e4,round(mean(k.5),digits=3), col = "red")
