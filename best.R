## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. (https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip)

## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”
best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv")
  
  suppressWarnings(outcomedata[, 11] <- as.numeric(as.character(outcomedata[, 11])))
  suppressWarnings(outcomedata[, 17] <- as.numeric(as.character(outcomedata[, 17])))
  suppressWarnings(outcomedata[, 23] <- as.numeric(as.character(outcomedata[, 23])))
  
  uniquestate <- unique(outcomedata$State)
  
  ## Check that state
  if (!state %in% uniquestate){
    stop("invalid state")
  }
  
  ## Check that outcome are valid
  outcomeDF <- data.frame(Out = c("heart attack", "heart failure", "pneumonia"), 
                          Col = c(11, 17, 23))
  
  nOut <- outcomeDF[outcomeDF$Out == outcome, ]
  
  if (nrow(nOut) == 0){
    stop("invalid outcome")
  }
  
  #Create new data frame with 3 col
  newoutcome <- data.frame(Hospital = outcomedata$Hospital.Name, State = outcomedata$State, Out = outcomedata[, nOut$Col])
  
  #Select the state
  newoutcome <- newoutcome[newoutcome$State == state, ]
  
  #Get the min number of mortality
  minmortality <- min(newoutcome[, 3], na.rm = TRUE)
  
  newoutcome <- newoutcome[newoutcome$Out == minmortality, ]
  anws <- sort(as.vector(newoutcome$Hospital))
  
  ## Return hospital name in that state with lowest 30-day death
  anws[1]
}
