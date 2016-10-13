corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the  
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations 
        ## NOTE: Do not round the result!
        
        list_files <- list.files(directory, full.names = TRUE)
        df <- data.frame()
        df_completed <- complete(directory)
        df_filtered <- df_completed[which(df_completed$nobs > threshold), ]
        result <- vector()
        for (i in df_filtered$id) {
                df <- read.csv(list_files[i], header = TRUE)
                result <- c(result, cor(df$nitrate, df$sulfate, use = "complete.obs"))
        }
        result
}