library(tm)
library(SnowballC)
library(RWeka)

readData <- function(fileName) {
    ## Define ngram arrays
    unigrams <- data_frame()
    bigrams <- data_frame()
    trigrams <- data_frame()
  
  
    ## Get the number of lines
    com <- paste("wc -l ", fileName, " | awk '{ print $1 }'", sep="")
    numLines <- system(command=com, intern=TRUE)
    
    ## Open Connections
    con <- file(fileName, "r") 
    
    numLines <- 100
    
    
    ## Loop over file
    for (i in 1:numLines) {
        ## do something on a line of data 
        line <- tolower(readLines(con, 1))
        line <- gsub("[^[:alnum:][:space:]]", "", line)
      
        w <- strsplit(line, " ", fixed = TRUE)[[1L]]
        
        ## Word tri-grams pasted together:
        ## unigrams <- append(unigrams, vapply(ngrams(w, 1L), paste, "", collapse = " "))
        ## bigrams <- append(bigrams, vapply(ngrams(w, 2L), paste, "", collapse = " "))
        ## trigrams <- append(trigrams, vapply(ngrams(w, 3L), paste, "", collapse = " "))
    
        unigrams <- rbind(unigrams, ngrams(w, 1L))
        bigrams <- rbind(bigrams, ngrams(w, 2L))
        trigrams <- rbind(trigrams, ngrams(w, 3L))
    }
    
    ## Close connection
    close(con)
    
    ## list(unlist(unigrams), unlist(bigrams), unlist(trigrams))
    list((unigrams), (bigrams), (trigrams))
}

analyzeData <- function(result) {
 
  t1 <- table(result[[1]])
  dens <- density(t1)
  
  ## hist(t1, breaks = 25)
  plot(dens)
  
  low <- quantile(t1, 0.0)
  high <- quantile(t1, 0.90)
  
  x1 <- min(which(dens$x >= low))  
  x2 <- max(which(dens$x <  high))
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="steelblue"))
}

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
    
    corpus
    
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
}

ngram <- function(outputDir, dataSource) {
  corpus <- readRDS(paste(outputDir, "/", dataSource  ,"Corpus.RData", sep = ""))
  
  corpus <- data.frame(unlist(sapply(corpus,`[`, "content")), stringsAsFactors = FALSE)
  
  makeNgram <- function(corpus, min, max) {
      n <- NGramTokenizer(corpus, control = Weka_control(max = max, min = min))
      n <- data.frame(table(n))
      colnames(n) <- c("ngram", "count")
      
      n
  }
  
  ## Contruct the ngrams
  unigram <- makeNgram(corpus, 1, 1)
  bigram <- makeNgram(corpus, 2, 2)
  trigram <- makeNgram(corpus, 3, 3)
  tetragram <- makeNgram(corpus, 4, 4)
  pentagram <- makeNgram(corpus, 5, 5)
  
  ## save the ngrams
  saveRDS(unigram, file = paste(outputDir, "/", dataSource, "-unigram.RData", sep = ""))
  saveRDS(bigram, file = paste(outputDir, "/", dataSource, "-bigram.RData", sep = ""))
  saveRDS(trigram, file = paste(outputDir, "/", dataSource, "-trigram.RData", sep = ""))
  saveRDS(tetragram, file = paste(outputDir, "/", dataSource, "-tetragram.RData", sep = ""))
  saveRDS(pentagram, file = paste(outputDir, "/", dataSource, "-pentagram.RData", sep = ""))
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


