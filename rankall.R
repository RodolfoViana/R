## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates.

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv")
  
  suppressWarnings(outcomedata[, 11] <- as.numeric(as.character(outcomedata[, 11])))
  suppressWarnings(outcomedata[, 17] <- as.numeric(as.character(outcomedata[, 17])))
  suppressWarnings(outcomedata[, 23] <- as.numeric(as.character(outcomedata[, 23])))
  
  ## Check that outcome are valid
  outcomeDF <- data.frame(Out = c("heart attack", "heart failure", "pneumonia"), 
                          Col = c(11, 17, 23))
  
  nOut <- outcomeDF[outcomeDF$Out == outcome, ]
  
  if (nrow(nOut) == 0){
    stop("invalid outcome")
  }
  
  # Create new data frame with 3 col
  newoutcome <- data.frame(Hospital = outcomedata$Hospital.Name, State = outcomedata$State, Out = outcomedata[, nOut$Col])
  newoutcome <- newoutcome[complete.cases(newoutcome),]
  
  suppressWarnings(newoutcome[,1] <- as.character(newoutcome[,1]))
  suppressWarnings(newoutcome[,2] <- as.character(newoutcome[,2]))
  suppressWarnings(newoutcome[,3] <- as.numeric(as.character(newoutcome[,3])))
  
  if (num=="best"){
    numaux <- 1
  } 
  
  name <- character(0)
  
  ## For each state, find the hospital of the given rank
  for (state in sort(unique(newoutcome$State))){  
    aux <- newoutcome[newoutcome$State == state, ]
    aux <- aux[order(aux$Out, aux$Hospital),]
    if (num=="worst"){
      numaux <- nrow(aux)
    }
    aux <- aux[numaux,]
    name <- c(name, as.character(aux$Hospital))
  }
  
  ## Return a data frame with the hospital names and the
  anws <- data.frame(hospital = name, state = sort(unique(newoutcome$State)))
  anws  
}