## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

# Data at https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip

pollutantmean <- function(directory, pollutant, id = 1:332) {
        meanPollutant <- vector()
        for (i in id){
                path <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep=""))
                meanPollutant <- c(meanPollutant, path[, pollutant])
        }
        mean(meanPollutant, na.rm = TRUE)       
}



