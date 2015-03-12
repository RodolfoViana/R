## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

# Data at https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip

corr <- function(directory, threshold = 0) {
        anws <- numeric(0)
        
        for (i in 1:332){
                path <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep=""))
                df <- path[complete.cases(path),]
                n_row <- nrow(df)
                if (n_row > threshold){
                        anws <- c(anws, cor(df[, "sulfate"], df[, "nitrate"]))
                }      
        } 
        anws
}