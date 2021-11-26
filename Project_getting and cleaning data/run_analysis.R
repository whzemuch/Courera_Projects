# The purpose of this project is to demonstrate your ability to collect, 
#  work with, and clean a data set. The goal is to prepare tidy data
#  that can be used for later analysis.



library(data.table)
library(tidyverse)

# Task 1: Merge the training and the test data sets to one data sets--------
  
  # download and unzip files
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile = "dataFiles.zip")
  unzip(zipfile = "dataFiles.zip")
  
  # read in X_train.txt and y_train.txt and combined them by column
  X_train <- fread("./UCI HAR Dataset/train/X_train.txt")
  y_train <- fread("./UCI HAR Dataset/train/y_train.txt", col.names = "activity")
  
  train_data <- bind_cols(list(X_train, y_train), .id="train_data")
  dim(train_data)
  train_data[1:5, c(1:2, 562:563)]
    
  # read in X_test.txt and y_train.txt and combined them by column
  X_test <- fread("./UCI HAR Dataset/test/X_test.txt")
  y_test <- fread("./UCI HAR Dataset/test/y_test.txt", col.names = "activity")
  
  test_data <- bind_cols(list(X_test, y_test), .id="test_data")
  dim(test_data)
  test_data[1:5, c(1:2, 562:563)]

  # merge the train and test data by row
  merge_dataset <-
    rbindlist(list(train_data, test_data)) %>% 
    rename(data_source = .id)
  dim(merge_dataset)
  merge_dataset[1:5, c(1:2, 562:563)]


# Task 3: Uses descriptive activity names to name the activities in the data set--------

  activity_labels <- fread("activity_labels.txt", data.table = FALSE) %>%  deframe()
  activity_labels
  
  # add a column for the subject id
  merge_dataset <-  mutate(merge_dataset, subject_id = paste("subject", row_number(), sep = "_"))
  # decode the activity column
  merge_dataset <-  mutate(merge_dataset, activity = activity_labels[activity])
  merge_dataset[1:5, c(1:2, 562:564)] 
     


# Task 4: Appropriately labels the data set with descriptive variable names.--------
    
  # read in the labels for all the features
  features <- fread("features.txt", data.table = FALSE) %>% deframe()

  # rename the columns with feature labels.
  feature_idx = grep("V", colnames(merge_dataset))
  colnames(merge_dataset)[feature_idx] <- unname(features) 

  dim(merge_dataset)
  merge_dataset[1:5, c(1:2, 562:564)]



# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement--------
  
merge_dataset_mean_std <-  select(merge_dataset, subject_id, activity, data_source, contains(c("-mean","-std"))) 
merge_dataset_mean_std[1:10, 1:5]


# Task 5: create a tidy data set with average of each variable for each activity and and each subject--------

domain_signal_label <- setNames(c("frequency", "time"), c("f","t"))

tidy_dataset <- 
  merge_dataset_mean_std %>% 
    select(subject_id, activity, data_source, contains("-mean")) %>% 
    pivot_longer(
      cols= contains("-mean"),
      names_to = "measurement",
      values_to = "value"
    ) %>% 
    
    separate(col=measurement, into = c("item", "statistic", "direction"), sep="-" ) %>% 
    separate(item, into = c("signal_domain", "measurement"), sep =1) %>% 
    mutate(signal_domain= domain_signal_label[signal_domain]) %>% 
    select(subject_id, activity, measurement, signal_domain, statistic, direction, value, data_source) 

head(tidy_dataset, 20)
tail(tidy_dataset, 10)

# Save the tidy data set as a csv file
write.table(tidy_dataset,file = "tidyDataset.txt", row.names = FALSE)bd3f1745ce915fd16e1dec8b72fc21562cfdb441
fwrite(tidy_dataset, file="tidyDataSet.csv")

