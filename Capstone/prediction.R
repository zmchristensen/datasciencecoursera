

loadNgrams <- function(dir) {
  types <- c("blogs", "news", "tweets")
  
  uni <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-unigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-unigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-unigram.RData", sep = ""))
  )
  bi <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-bigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-bigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-bigram.RData", sep = ""))
  )
  tri <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-trigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-trigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-trigram.RData", sep = ""))
  )
  tetra <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-tetragram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-tetragram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-tetragram.RData", sep = ""))
  )
  penta <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-pentagram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-pentagram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-pentagram.RData", sep = ""))
  )  
}

preprocessGram <- function(fileName) {
  n <- as.data.frame(readRDS(fileName), stringsAsFactors = FALSE)
  
  s <- strsplit(as.character(n[1,1]), split = " ")[[1]]
  
  names <- c()
  for (i in 1:length(s)) {
    names <- c(names, i)
  }
  
  if (length(s) > 1) {
    n <- cbind(colsplit(n$ngram, split = " ", names = names), n$count)    
  }
  
  colnames(n) <- c(names, "count")
  n
}

sanitize <- function(text) {
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
  ## corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  ## remove numbers, punctuation, and whitespace
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  
  ## Stem the document with SnowballC
  corpus <- tm_map(corpus, stemDocument)
  corpus
}

predict <- function(phrase) {
  
  phrase <- sanitize(phrase)[[1]]$content
  words <- strsplit(x = phrase, split = " ")[[1]]
  length <- length(words)

  if (length == 0) {
     "NA"
  }
  else if (length == 1) {
    matches <- filter(bi, bi["1"] == words[1])
    filter(matches, count == max(count))      
  }
  else if (length == 2) {
    matches <- filter(tri, tri["1"] == words[1], tri["2"] == words[2])
    max_match <- filter(matches, count == max(count))      
    
    second <- filter(bi, bi["1"] == words[2])
    max_second <- cbind("", filter(second, count == max(count)))
    colnames(max_second) <- colnames(max_match)
    
    rbind(max_match, max_second)
  }
  else if (length == 3) {
    matches <- filter(tetra, tetra["1"] == words[1], tetra["2"] == words[2], tetra["3"] == words[3])
    max_match <- filter(matches, count == max(count))
        
    second <- filter(tri, tri["1"] == words[2], tri["2"] == words[3])
    max_second <- cbind("", filter(second, count == max(count)))   
    colnames(max_second) <- colnames(max_match)
    
    third <- filter(bi, bi["1"] == words[3])
    max_third <- cbind("", "", filter(third, count == max(count)))
    colnames(max_third) <- colnames(max_match)
    
    rbind(max_match, max_second, max_third)
  }
  else {
    words <- c(words[length(words) - 2], words[length(words) - 1], words[length(words)])
    
    matches <- filter(tetra, tetra["1"] == words[1], tetra["2"] == words[2], tetra["3"] == words[3])
    max_match <- filter(matches, count == max(count))
    
    second <- filter(tri, tri["1"] == words[2], tri["2"] == words[3])
    max_second <- cbind("", filter(second, count == max(count)))     
    colnames(max_second) <- colnames(max_match)
    
    third <- filter(bi, bi["1"] == words[3])
    max_third <- cbind("", "", filter(third, count == max(count)))
    colnames(max_third) <- colnames(max_match)
    
    rbind(max_match, max_second, max_third)
  }
}