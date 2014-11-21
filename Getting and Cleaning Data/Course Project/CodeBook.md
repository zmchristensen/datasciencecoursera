# CodeBook


### Overview

The <strong>run_analysis.R</strong> script reads data from the <strong>test</strong> and <strong>train</strong> folders produces two output files, <strong>full_output.txt</strong> and <strong>summary_output.txt</strong>.

- <strong>full_output.txt</strong> contains the full contents of the merged <strong>test</strong> and <strong>train</strong> datasets. There are multiple entries for each subject and activity, with each entry representing a sampling of data from the smartphone.
- <strong>summary_output.txt</strong> summarizes the data from <strong>full_output.txt</strong>. The mean of each variable for each subject and activity is computed and contained in one row.

### <br>

## Analysis Steps

1. Merges the training and the test sets to create one data set.
````r
full_X <- rbind(tbl_df(read.table(test_files[3])), tbl_df(read.table(train_files[3])))
````

- Extracts only the measurements on the mean and standard deviation for each measurement.
  ````r
  colnames <- colnames(full_data)
  keep_headers <- grepl("Subject", colnames) |
  grepl("Activity", colnames) |
  grepl("mean", colnames) |
  grepl("std", colnames)
  final_data <- full_data[keep_headers]
  ````

- Uses descriptive activity names to name the activities in the data set
  ````r
  final_colnames <- colnames(final_data)
  for (i in 1:length(final_colnames)) {
    final_colnames[i] = gsub("-", " ", final_colnames[i])
    final_colnames[i] = gsub("std\\()", "StdDev", final_colnames[i])
    final_colnames[i] = gsub("mean\\()", "Mean", final_colnames[i])
    final_colnames[i] = gsub("meanFreq\\()", "MeanFrequency", final_colnames[i])  
  }
  ````

- Appropriately labels the data set with descriptive variable names.
  ````r
  colnames(final_data) <- final_colnames
  ````

- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
````r
grouped_data <- final_data %>% aggregate(by = list(final_data$Subject, final_data$Activity), mean)
grouped_data$Activity <- activity_labels$V2[match(grouped_data$Activity, activity_labels$V1)]
grouped_data <- grouped_data[,3:ncol(grouped_data)]
````
````r
print(paste("Writing to", summary_file))
write.table(grouped_data, file = summary_file, row.names = FALSE)
````

### <br>

## Variables and Data Files

- The <strong>activity_labels.txt</strong> file maps the activity ID to a description of the activity.
  ````r
  activity_labels <- tbl_df(read.table("activity_labels.txt"))
  ````

- The <strong>features.txt</strong> file contains the columns names for the <strong>X_[type].txy</strong> files, with more information about the variables discussed in <strong>features_info.txt</strong>
  ````r
  features <- read.table("features.txt")[2]
  ````

- Both the <strong>test</strong> and <strong>train</strong> datasets contain 3 applicable files: <strong>subject_[type].txt</strong>, <strong>Y_[type].txt</strong>, and <strong>X_[type].txt</strong>.
  ````r
  test_files <- c("test/subject_test.txt", "test/Y_test.txt", "test/X_test.txt")
  train_files <- c("train/subject_train.txt", "train/Y_train.txt", "train/X_train.txt")
  ````

  - <strong>subject_[type].txt</strong> Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
    ````r
    full_subject <- rbind(tbl_df(read.table(test_files[1])), tbl_df(read.table(train_files[1])))
    colnames(full_subject) <- c("Subject")
    ````

  - <strong>Y_[type].txt</strong> contains the labels for the type of activity being measured for each window sample. Its range is from 1 to 6.
    ````r
    full_Y <- rbind(tbl_df(read.table(test_files[2])), tbl_df(read.table(train_files[2])))
    colnames(full_Y) <- c("Activity")
    ````

  - <strong>X_[type].txt</strong> contains the data points for each window sample.
    ````r
    full_X <- rbind(tbl_df(read.table(test_files[3])), tbl_df(read.table(train_files[3])))
    colnames(full_X) <- features$V2
    ````
