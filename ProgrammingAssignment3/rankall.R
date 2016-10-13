rankall <- function(outcome, ranking = "best") {
        
        ## Read outcome data
        setwd("~/datasciencecoursera/R-Programming/ProgrammingAssignment3")
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # change data type from character to numeric
        df[, 11] <- as.numeric(df[, 11]) # heart attack
        df[, 17] <- as.numeric(df[, 17]) # heart failure
        df[, 23] <- as.numeric(df[, 23]) # pneumonia

        
        
        ## The outcomes can be one of “heart attack”, “heart failure”, or
        ## “pneumonia”.
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        states <-  unique(df[,7])
        states_sorted <- sort(states)
        result  <- data.frame()
        for (i in seq_along(states_sorted)) {
                hosp_name <- rankhospital(states_sorted[i], outcome, ranking)
                result <- rbind(result, data.frame("hospital name" = hosp_name,
                                                   "state" = states_sorted[i]))
        }
        result
        
        
}

