set.seed(2015)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)

download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "training.csv")
training <- read.csv("training.csv")

inTrain <- createDataPartition(training$classe, p = 0.05, list = FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

train <- train[,-c(1:7)]
test <- test[,-c(1:7)]

## Compute the percentage of NA values in each column and print as a table
na_count <- sapply(train, function (n) { sum(is.na(n))})
na_table <- as.data.frame(table(na_count))
na_table

## Remove columns that contain mostly NAs
badColumns <- c()
for (i in 1:ncol(train)) {
  if (sum(is.na(train[,i])) == na_table[2,1]) {
    badColumns <- c(badColumns, i)
  }
}
train <- train[,-badColumns]
test <- test[,-badColumns]

## Remove columns that have Near Zero Variance
nzv <- nearZeroVar(train, saveMetrics = TRUE)
train <- train[,-which(nzv$nzv)]
test <- test[,-which(nzv$nzv)]

## Train with method = "rpart"
rpartModel <- train(classe ~ ., train, method = "rpart")
rfModel <- train(classe ~ ., train, method = "rf")



