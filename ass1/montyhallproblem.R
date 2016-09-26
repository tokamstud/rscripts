###--- Monty-Hall-problem
# Auuthor: Tomasz gliniecki

keep <-0;
switch <-0;
possible_items <- c("car","goat0","goat1");
possible_choices <- c(1,2,3);

n <- 1e4;

# calculate probability for winning with strategy 1 & 2
for (i in c(0:n)) {
    gate_distribution <- sample(possible_items, prob=c(1/3,1/3,1/3));
    contestant_choice <- sample(possible_choices,size=1,prob=c(1/3,1/3,1/3));

    # amount of wins strategy 1
    if (gate_distribution[contestant_choice] == "car") {
       keep <- keep+1;
    }

    # amount of wins strategy 2
    if ("car" %in% gate_distribution[-contestant_choice]) {
       switch <- switch+1;
    }
}

print(switch/n);
