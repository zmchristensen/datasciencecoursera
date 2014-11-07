complete <- function(directory, id = 1:332) {
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  
  
  ids <- vector()
  counts <- vector()
  
  for (i in id) {
    file <- paste(directory, "/", sprintf("%03d.csv", i), sep = "")
    
    data <- read.csv(file)
    totalGood <- !is.na(data["nitrate"]) & !is.na(data["sulfate"])
    
    ids <- c(ids, i)
    counts <- c(counts, sum(totalGood))
  }   
  
  
  frame <- data.frame(ids, counts)
  colnames(frame) <- c("id", "nobs")
  
  frame
}