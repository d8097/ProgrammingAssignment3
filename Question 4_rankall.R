## Saved code for rankall
## from http://xmuxiaomo.github.io/2015/06/19/R-Programming-Assignment-3/
##Question 4 Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv")
        
## Rename variables
if(outcome == "heart attack"){
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromHeartAttack'] <- 'OutcomeRate'
} else if(outcome == "heart failure"){
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromHeartFailure'] <- 'OutcomeRate'
} else {
names(csv)[names(csv) == 'Hospital30DayDeathMortalityRatesfromPneumonia'] <- 'OutcomeRate'
}

        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Hospital of the given rank by state
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        
        ## Make empty vector then fill
        output <- vector()
        
        states <- levels(data[, 7])
        
        for(i in 1:length(states)) {
                statedata <- data[grep(states[i], data$State), ]
                orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                             na.last = NA), ]
                hospital <- if(num == "best") {
                        orderdata[1, 2]
                } else if(num == "worst") {
                        orderdata[nrow(orderdata), 2]
                } else{
                        orderdata[num, 2]
                }
                output <- append(output, c(hospital, states[i]))
        }

        ## Data frame with hospital name
        output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(output) <- c("hospital", "state")
        rownames(output) <- states
        
        output
}
