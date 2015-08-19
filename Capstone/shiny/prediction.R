
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

predict <- function(phrase, uni, bi, tri, tetra, penta) {
  
  phrase <- sanitize(phrase)[[1]]$content
  words <- strsplit(x = phrase, split = " ")[[1]]
  length <- length(words)

  matches <- data.frame()
  if (length == 0 || (is.null(bi) && is.null(tri) && is.null(tetra) && is.null(penta))) {
     "NA"
  }
  else {
    if (length >= 1 && !is.null(bi)) {
      ## match against bi words and take last two columns
      bi_matches <- filter(bi, bi["1"] == words[length])[,c(2:3)] %>% mutate("source" = "bi")
      colnames(bi_matches) <- c("prediction", "count", "source")
      matches <- rbind(matches, bi_matches)
    }
    if (length >= 2 && !is.null(tri)) {
      ## match against tri and take the last two columns
      tri_matches <- filter(tri, tri["1"] == words[length - 1], tri["2"] == words[length])[,c(3:4)] %>% mutate("source" = "tri")
      colnames(tri_matches) <- c("prediction", "count", "source")
      matches <- rbind(matches, tri_matches)
    }
    if (length >= 3 && !is.null(tetra)) {
      ## match against tetra and take the last two columns
      tetra_matches <- filter(tetra, tetra["1"] == words[length - 2], tetra["2"] == words[length - 1], tetra["3"] == words[length])[,c(4:5)] %>% mutate("source" = "tetra")
      colnames(tetra_matches) <- c("prediction", "count", "source")
      matches <- rbind(matches, tetra_matches)
    }
    if (length >= 4 && !is.null(penta)) {
      penta_matches <- filter(penta, penta["1"] == words[length - 3], penta["2"] == words[length - 2], penta["3"] == words[length - 1], penta["4"] == words[length])[,c(5:6)] %>% mutate("source" = "penta")
      colnames(penta_matches) <- c("prediction", "count", "source")
      matches <- rbind(matches, penta_matches)
    }
    
    r <- matches %>% group_by(prediction) %>% summarise(count = sum(count))
    colnames(r) <- c("prediction", "count")
    r 
  }
}