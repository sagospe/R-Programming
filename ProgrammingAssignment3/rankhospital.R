## Auxiliar function to retrieve the hospital name based in col_index of
## selected outcome
get_hospital <- function(data, col_index, rank) {
        ## Ordering the data frame
        data_ordered <- data[order(data[col_index], data$Hospital.Name, na.last = NA), ]
        num <- nrow(data_ordered)
        ## Checking what value stored in rank
        if (rank == "best") {
                data_ordered[1, 2]
        } else if (rank == "worst") {
                data_ordered[num, 2]
        } else if (rank > num) {
                NA 
        } else data_ordered[rank, 2]
}


rankhospital <- function(state, outcome, ranking = "best") {
        
        ## Read outcome data
        setwd("~/datasciencecoursera/R-Programming/ProgrammingAssignment3")
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # change data type from character to numeric
        df[, 11] <- as.numeric(df[, 11]) # heart attack
        df[, 17] <- as.numeric(df[, 17]) # heart failure
        df[, 23] <- as.numeric(df[, 23]) # pneumonia
        
        ## Check that state and outcome are valid
        if (!state %in% unique(df[,7])) {
                stop("invalid state")
        }
        
        ## The outcomes can be one of “heart attack”, “heart failure”, or
        ## “pneumonia”.
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        df_filtered_state <- data.frame()
        df_filtered_state <- subset(df, State == state)
        if (outcome == "heart attack") {
                hosp_name <- get_hospital(df_filtered_state, 11, ranking)
                
        } else if (outcome == "heart failure") {
                hosp_name <- get_hospital(df_filtered_state, 17, ranking)
                
        }
        else if (outcome == "pneumonia") {
                hosp_name <- get_hospital(df_filtered_state, 23, ranking)
          
        }
        hosp_name
        
}

