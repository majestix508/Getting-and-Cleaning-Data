# Getting-and-Cleaning-Data Course Project
---
#Overview

From the coursera webpage:

>The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

>One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

>http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

>Here are the data for the project:

>https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

>You should create one R script called run_analysis.R that does the following.

>- Merges the training and the test sets to create one data set.
- Extracts only the measurements on the mean and standard deviation for each measurement.
- Uses descriptive activity names to name the activities in the data set
- Appropriately labels the data set with descriptive variable names.
- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

---
## How to run the script
In this repository exists a file called `run_analysis.R` which contains a function `run_analysis()`.
1. Download the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. unzip the data and place the `run_analysis.R` script in the same directory where the extracted directory (`UCI HAR Dataset`) is.
3. set the working directory to the directory where `run_analysis.R` is
4. `source("run_analysis.R")`
5. run the function with `run_analysis()`
6. Wait for the function to finish and the result is a new file called `tidy.txt` in the same directory where `run_anlysis.R` is

If you don't want to download and extract the data yourself, just start the script and it will download and extract the data in the working directory.
