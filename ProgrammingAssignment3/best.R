## Auxiliar function to retrieve the hospital name based in col_index of
## selected outcome
get_hospital <- function(data, col_index) {
        lowest <- min(data[, col_index], na.rm = TRUE)
        df_filtered <- data[which(data[, col_index] == lowest), ]
        df_filtered[order(df_filtered$Hospital.Name), 2][1]
}


best <- function(state, outcome) {
        
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
                hosp_name <- get_hospital(df_filtered_state, 11)
                
        } else if (outcome == "heart failure") {
                hosp_name <- get_hospital(df_filtered_state, 17)
                
        }
        else if (outcome == "pneumonia") {
                hosp_name <- get_hospital(df_filtered_state, 23)
          
        }
        hosp_name
        
}

