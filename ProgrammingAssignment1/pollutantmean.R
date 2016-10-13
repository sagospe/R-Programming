pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        
        ## 'pollutant' is a character vector of length 1 indicating 
        ## the name of the pollutant for which we will calculate the 
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is a integer vector indicating the monitor ID numnbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        list_files <- list.files(directory, full.names = TRUE)
        df <- data.frame()
        for (i in id) {
                df <- rbind(df, read.csv(list_files[i], header = TRUE))
        }
        mean(df[, pollutant], na.rm = TRUE)
        
        
}