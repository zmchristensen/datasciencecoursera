q5 <- function(fileName) {
  ## Open Connections
  con <- file(fileName, "r") 
  
  
  ## Get the number of lines
  com <- paste("wc -l ", fileName, " | awk '{ print $1 }'", sep="")
  numLines <- system(command=com, intern=TRUE)
  
  numLines <- 1000000
  
  match <- "NULL"
  
  ## Loop over file
  for (i in 1:numLines) {
    
      ## do something on a line of data 
      line <- tolower(readLines(con, 1, skipNul = TRUE))
      if (grepl("biostats", line, ignore.case = TRUE)) {
          match <- line
      }
  }
  
  ## Close connection
  close(con)
  
  match  
}

q3 <- function(fileName) {
  ## Open Connections
  con <- file(fileName, "r") 
  
  
  ## Get the number of lines
  com <- paste("wc -l ", fileName, " | awk '{ print $1 }'", sep="")
  numLines <- system(command=com, intern=TRUE)
  
  max <- 0
  
  ## Loop over file
  for (i in 1:numLines) {
    
    ## do something on a line of data 
    line <- readLines(con, 1, skipNul = TRUE)
    count <- nchar(line)
    
    if (count > max) {
      max <- count
    }
    
  }
  
  ## Close connection
  close(con)
  
  paste(fileName, max, sep = " : ")
}


q6 <- function(fileName) {
  ## Open Connections
  con <- file(fileName, "r") 
  
  
  ## Get the number of lines
  com <- paste("wc -l ", fileName, " | awk '{ print $1 }'", sep="")
  numLines <- system(command=com, intern=TRUE)
  
  ## numLines <- 1000000
  match <- ""
  pattern <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
  
  ## Loop over file
  for (i in 1:numLines) {
    
    ## do something on a line of data 
    line <- tolower(readLines(con, 1, skipNul = TRUE))
    if (grepl(pattern, line, ignore.case = TRUE)) {
      match <- line
      break
    }
  }
  
  ## Close connection
  close(con)
  
  match  
}

q4 <- function(fileName) {
  ## Open Connections
  con <- file(fileName, "r") 
  
  
  ## Get the number of lines
  com <- paste("wc -l ", fileName, " | awk '{ print $1 }'", sep="")
  numLines <- system(command=com, intern=TRUE)
  
  ## numLines <- 1000000
  love <- 0
  hate <- 0
  l_pattern <- "love"
  h_pattern <- "hate"
  
  ## Loop over file
  for (i in 1:numLines) {
    
    ## do something on a line of data 
    line <- tolower(readLines(con, 1, skipNul = TRUE))
    if (grepl(l_pattern, line, ignore.case = TRUE)) {
      love <- love + 1
    }
    if (grepl(h_pattern, line, ignore.case = TRUE)) {
      hate <- hate + 1
    }
  }
  
  ## Close connection
  close(con)
  
  love / hate  
}