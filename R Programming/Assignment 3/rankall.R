rankall <- function(outcome, num = "best") {
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
  
  states <- unique(outcomes["State"])
  states <- states[order(states["State"]),]
  
  names <- vector()
  abbrevs <- vector()
  
  for (i in 1:length(states)) {
    ## Check that the state is valid
    records <- subset(outcomes, subset = (outcomes$State == states[i]))
    if (nrow(records) == 0) {
      stop("invalid state")
    }
    
    ## Convert the necessary column to numeric
    records[, colIndex] <- as.numeric(records[, colIndex])
    validCount <- sum(!is.na(records[colIndex]))
    
    index <- num
    if (num == "best") {
      index = 1
    }
    else if (num == "worst") {
      index = validCount
    }
    
    if (index > validCount) {
      names <- c(names, NA)
      abbrevs <- c(abbrevs, states[i]) 
    }
    else {
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      ordered <- records[order(records[colIndex], records[2], na.last = TRUE),]
      result<- c(as.character(ordered[index, 2]))    
      names <- c(names, result)
      abbrevs <- c(abbrevs, states[i])
    }
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  final <- data.frame(names, abbrevs)
  colnames(final) <- c("hospital", "state")
  
  final
}