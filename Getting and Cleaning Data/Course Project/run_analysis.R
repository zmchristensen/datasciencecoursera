library(dplyr)

activity_labels <- tbl_df(read.table("activity_labels.txt"))
features <- read.table("features.txt")[2]

test_files <- c("test/subject_test.txt", "test/Y_test.txt", "test/X_test.txt")
train_files <- c("train/subject_train.txt", "train/Y_train.txt", "train/X_train.txt")


create_tidy_data <- FALSE
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
  full_Y <- left_join(full_Y, activity_labels)
  colnames(full_Y) <- c("Act_ID", "Activity")
  
  
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
  
  ## Write the output file  
  output_file <- "tidy_output.txt"
  print(paste("Writing to", output_file))
  write.table(final_data, file = output_file, row.names = FALSE)

}



