##Saved code for best.R
##from http://xmuxiaomo.github.io/2015/06/19/R-Programming-Assignment-3/
##Question 2 Finding the best hospital in a state

best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv")
        
## Rename variables
if(outcome == "heart attack"){
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromHeartAttack'] <- 'OutcomeRate'
} else if(outcome == "heart failure"){
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromHeartFailure'] <- 'OutcomeRate'
} else {
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromPneumonia'] <- 'OutcomeRate'
}

        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Hospital name in that state with lowest 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}
