library(dplyr)

activity_labels <- tbl_df(read.table("activity_labels.txt"))
features <- read.table("features.txt")[2]

test_files <- c("test/subject_test.txt", "test/Y_test.txt", "test/X_test.txt")
train_files <- c("train/subject_train.txt", "train/Y_train.txt", "train/X_train.txt")

output_file <- "tidy_output.txt"
summary_file <- "summary.txt"


create_tidy_data <- TRUE
print(paste("Create Tidy Data?", create_tidy_data))

## Only execute this code of the tidy_output.txt file needs to be generated
if (create_tidy_data) {

  ## Get the Subject for each row
  print("Subject...")
  full_subject <- rbind(tbl_df(read.table(test_files[1])), tbl_df(read.table(train_files[1])))
  colnames(full_subject) <- c("Subject")
  
  
  ## Get the Acitivity for each row and substitute activity name
  print("Activity...")
  full_Y <- rbind(tbl_df(read.table(test_files[2])), tbl_df(read.table(train_files[2])))
  colnames(full_Y) <- c("Activity")
  
  
  ## Get the data for each row
  print("Data Points...")
  full_X <- rbind(tbl_df(read.table(test_files[3])), tbl_df(read.table(train_files[3])))
  colnames(full_X) <- features$V2
  
  
  ## Create full dataset
  full_data <- cbind(full_subject, full_Y, full_X)


  ## Create logical vector of columns to keep
  ## Only keeping columns containing Subject, Activtiy, mean, or std
  colnames <- colnames(full_data)
  keep_headers <- grepl("Subject", colnames) | grepl("Activity", colnames) | grepl("mean", colnames) | grepl("std", colnames)
  final_data <- full_data[keep_headers]
  
  final_colnames <- colnames(final_data)
  
  for (i in 1:length(final_colnames)) {
#    final_colnames[i] = gsub("\\()", "", final_colnames[i])
    final_colnames[i] = gsub("-", " ", final_colnames[i])
    final_colnames[i] = gsub("std\\()", "StdDev", final_colnames[i])
    final_colnames[i] = gsub("mean\\()", "Mean", final_colnames[i])
    final_colnames[i] = gsub("meanFreq\\()", "MeanFrequency", final_colnames[i])  
  }
  
  colnames(final_data) <- final_colnames

  
  ## TEST: Write Clean Column Names
  write.table(final_colnames, file = "clean_colnames.txt", row.names = FALSE)

  
  ## Write the output file  
  print(paste("Writing to", output_file))
  write.table(final_data, file = output_file, row.names = FALSE)

} else {
  ## If not generating the output_file, read data into final_data
  final_data <- tbl_df(read.table(output_file, header = TRUE))  
}


## Create summary file

grouped_data <- final_data %>% aggregate(by = list(final_data$Subject, final_data$Activity), mean)

## Add the activity names
grouped_data$Activity <- activity_labels$V2[match(grouped_data$Activity, activity_labels$V1)]

#remove the group column
grouped_data <- grouped_data[,3:ncol(grouped_data)]

## Write the output file  
print(paste("Writing to", summary_file))
write.table(grouped_data, file = summary_file, row.names = FALSE)



