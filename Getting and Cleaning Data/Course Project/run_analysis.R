library(dplyr)

activity_labels <- tbl_df(read.table("activity_labels.txt"))

test_files <- c("test/subject_test.txt", "test/Y_test.txt", "test/X_test.txt")
train_files <- c("train/subject_train.txt", "train/Y_train.txt", "train/X_train.txt")

## Get the data for each file
print(test_files[1])
full_subject <- rbind(tbl_df(read.table(test_files[1])), tbl_df(read.table(train_files[1])))
colnames(full_subject) <- c("Subject")

print(test_files[2])
full_Y <- rbind(tbl_df(read.table(test_files[2])), tbl_df(read.table(train_files[2])))
colnames(full_Y) <- c("Activity")

print(test_files[3])
full_X <- rbind(tbl_df(read.table(test_files[3])), tbl_df(read.table(train_files[3])))

## Create full dataset
final_data <- cbind(full_subject, full_Y, full_X)
print(dim(final_data))


## Only keep some columns