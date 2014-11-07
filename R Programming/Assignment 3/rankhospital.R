rankhospital <- function(state, outcome, num = "best") {
  ## Check that outcome is valid
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
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that the state is valid
  records <- subset(outcomes, subset = (outcomes$State == state))
  if (nrow(records) == 0) {
    stop("invalid state")
  }
  
  ## Convert the necessary column to numeric
  records[, colIndex] <- as.numeric(records[, colIndex])
  validCount <- sum(!is.na(records[colIndex]))
  
  if (num == "best") {
    num = 1
  }
  else if (num == "worst") {
    num = validCount
  }
  
  if (num > validCount) {
    NA
  }
  else {
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ordered <- records[order(records[colIndex], records[2], na.last = TRUE),]
    c(as.character(ordered[num, 2]))    
  }
  
}