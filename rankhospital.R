# This function takes three arguments: The 2-character abbreviated name of a
#state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
#of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
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
  # Create new data frame with 3 col
  newoutcome <- data.frame(Hospital = outcomedata$Hospital.Name, State = outcomedata$State, Out = outcomedata[, nOut$Col])
  
  # Select the state
  newoutcome <- newoutcome[newoutcome$State == state, ]
  newoutcome <- newoutcome[complete.cases(newoutcome),]
  
  # Get the rank of hospitals 
  newdata <- newoutcome[order(newoutcome$Out, newoutcome$Hospital, newoutcome$State),]
  
  if (num=="best"){
    anws <- newdata[1,]$Hospital
  } 
  if (num=="worst"){
    anws <- newdata[nrow(newdata),]$Hospital
  } else {
    anws <- newdata[num,]$Hospital
  }
  
  ## Return hospital name in that state with the given rank
  anws <- as.character(anws)
  anws
}