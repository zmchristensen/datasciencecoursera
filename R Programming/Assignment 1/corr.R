corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  results <- vector()
  nits <- vector()
  sulfs <- vector()
  
  # Get all the files in directory
  files <- list.files(directory, pattern = "*.*", full.names = TRUE)
  
  # Get all the data from the files
  for (file in files) {
    data <- read.csv(file)
    bools <- !is.na(data["nitrate"]) & !is.na(data["sulfate"])

    if (sum(bools) > threshold) {
      nits <- data["nitrate"][bools]
      sulfs <- data["sulfate"][bools]
      results <- c(results, cor(nits, sulfs))
    }
  }  
  results
}