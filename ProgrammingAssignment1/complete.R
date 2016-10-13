complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is a integer vector indicating the monitor ID numnbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        list_files <- list.files(directory, full.names = TRUE)
        df <- data.frame()
        df_completed <- data.frame()
        for (i in id) {
                df <- read.csv(list_files[i], header = TRUE)
                n <- nrow(df[!is.na(df$sulfate) & !is.na(df$nitrate), ])
                df_completed <- rbind(df_completed, data.frame(id = i, nobs = n))
        }
        df_completed
}