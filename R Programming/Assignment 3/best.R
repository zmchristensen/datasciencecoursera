best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that the state and outcome are valid
  
  ## Return hospital name in the state with the lowest 30-day death rate
  
  colIndex = -1
  if (outcome == "heart attack") {
    colIndex = 11
  }
  else if (outcome == "heart failure") {
    colIndex = 17
  }
  else if (outcome == "pneumonia") {
    colIndex = 23
  }
  else {
    stop("invalid outcome")
  }
  
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  records <- subset(outcomes, subset = (outcomes$State == state))
  if (nrow(records) == 0) {
    stop("invalid state")
  }
  
  records[, colIndex] <- as.numeric(records[, colIndex])
  records[colIndex][is.na(records[colIndex])] <- Inf
  min <- min(records[colIndex])
  
  hospitals <- subset(records, subset = records[colIndex] == min)
  c(as.character(hospitals[2]))
}