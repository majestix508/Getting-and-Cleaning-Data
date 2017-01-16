#Getting and Cleaning Data Course Project
run_analysis <- function() {
  
  library(plyr)
  
  #check if dir exists - if not then download data and unzip it!
  if (!file.exists("./UCI HAR Dataset")){
    originalDatasetUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    tmpFile <- tempfile(pattern="UCI-HAR-Dataset", tmpdir = ".", fileext = ".zip")
    download.file(originalDatasetUrl, tmpFile)
    unzip(tmpFile)
    unlink(tmpFile)
  }
  
  #read train and test data
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  #2 merge the data
  x_all <- rbind(X_train, X_test)
  y_all <- rbind(y_train, y_test)
  subject_all <- rbind(subject_train, subject_test)
  
  #3 read the features
  features <- read.table("UCI HAR Dataset/features.txt")
  
  # select only the means and standard deviations from data
  # use grep to get the column number of the features which contains 'std' or 'mean()'
  features_mean_std <- grep("std|mean\\(\\)", features[,2])
  
  # create a table with the features we want
  x_extract <- x_all[,features_mean_std]
  
  # set the column names
  names(x_extract) <- features[features_mean_std, 2]
  
  #name the activities in the data
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

  y_all[,1] <- activity_labels[y_all[,1], 2]
  
  names(y_all) <- "activity"
  
  
  
  # Appropriately labels the data set with descriptive variable names.
  names(subject_all) <- "subject"
  
  # bind data into a single data table
  all_data <- cbind(x_extract, y_all, subject_all)
  
  
  # From the data set in step 4, creates a second, independent tidy data set 
  # with the average of each variable for each activity and each subject.
  tidy <- ddply(.data = all_data, .variables = c("subject", "activity"), .fun = numcolwise(mean))
  
  write.table(tidy, "tidy.txt", row.names = FALSE)
}
