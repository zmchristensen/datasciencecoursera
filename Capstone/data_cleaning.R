library(tm)
library(SnowballC)
library(RWeka)
library(reshape)
library(dplyr)

prepData <- function() {
  
  ## download zip file if not present
  destFile <- "Coursera-SwiftKey.zip"
  if (!file.exists(destFile)) {
    downloadUrl <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(downloadUrl, "Coursera-SwiftKey.zip")    
  }
  
  ## expand zip archive if not done
  zipFile <- "final"
  if (!file.exists(zipFile)) {
    unzip(destFile)    
  }
}

## Read the file and save as corpus
read <- function(sourceFile, outputDir, dataSource, lines) {
    ## open the file, read the text into memory, and close connection
    con <- file(sourceFile)
    text <- readLines(con, lines)
    close(con)
    
    corpus <- Corpus(VectorSource(text))
    rm(text)
    
    ## Replace some special characters with a space
    toSpace <- content_transformer(function(x, pattern) {
      gsub(pattern, " ", x)
    })
    corpus <- tm_map(corpus, toSpace, "/|@|\\|")

    ## convert to lower case, remove numbers and punctuation
    corpus <- tm_map(corpus, content_transformer(tolower))
    
    ## remove stopwords before punctuation removed
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    ## remove numbers, punctuation, and whitespace
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    
    ## Stem the document with SnowballC
    corpus <- tm_map(corpus, stemDocument)
    
    ## Save corpus
    saveFile = paste(outputDir, "/", dataSource, "Corpus.RData", sep = "")
    saveRDS(corpus, file = saveFile)
    
    rm(toSpace)
    rm(saveFile)
    rm(corpus)
}

ngram <- function(outputDir, dataSource) {
  corpus <- readRDS(paste(outputDir, "/", dataSource  ,"Corpus.RData", sep = ""))
  
  corpus <- unlist(sapply(corpus,`[`, "content"))
  
  ## Contruct and save the ngrams
  makeNgram <- function(corpus, min, max, saveFile) {
      n <- NGramTokenizer(corpus, control = Weka_control(max = max, min = min))
      n <- data.frame(table(n))
      colnames(n) <- c("ngram", "count")
      
      saveRDS(n, file = saveFile)
      rm(n)
  }
  
  makeNgram(corpus, 1, 1, paste(outputDir, "/", dataSource, "-unigram.RData", sep = ""))
  makeNgram(corpus, 2, 2, paste(outputDir, "/", dataSource, "-bigram.RData", sep = ""))
  makeNgram(corpus, 3, 3, paste(outputDir, "/", dataSource, "-trigram.RData", sep = ""))
  makeNgram(corpus, 4, 4,paste(outputDir, "/", dataSource, "-tetragram.RData", sep = ""))
  makeNgram(corpus, 5, 5,paste(outputDir, "/", dataSource, "-pentagram.RData", sep = ""))
}

process <- function(fileNumber = 1, lines = 10000) {
  
  prepData()
  
  ## create the output directory
  output <- "output"
  dir.create(paste(getwd(), "/", output, sep = ""), showWarnings = FALSE)
  
  types <- c("blogs", "news", "tweets")
  sources <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")
  
  read(sources[fileNumber], output, types[fileNumber], lines)
  ngram(output, types[fileNumber])
}


