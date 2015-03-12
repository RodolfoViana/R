## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

# Data at https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip


complete <- function(directory, id = 1:332) {
        id_v <- vector()
        nobs_v <- vector()
        for (i in id){
                path <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep=""))
                nobs_v <- c(nobs_v, nrow(path[complete.cases(path),]))
                id_v <- c(id_v, i)
        }         
        data.frame("id" = id_v, "nobs" = nobs_v)
}
