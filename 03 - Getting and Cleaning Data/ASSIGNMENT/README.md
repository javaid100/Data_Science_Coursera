# Getting and Cleaning Data Course Project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to
prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no
questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a
Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the
data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should
also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they
are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article .
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new
users. The data linked to from the course website represent data collected from the accelerometers from the

Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### A R script called run_analysis.R is created and does the following:
### 1. Merges the training and the test sets to create one data set.
### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
### 3. Uses descriptive activity names to name the activities in the data set
### 4. Appropriately labels the data set with descriptive variable names.
### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Download data
library(data.table) <br />
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dat <br />
aset.zip' <br />
if (!file.exists('./UCI HAR Dataset.zip')) <br />
  { <br />
   download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb') <br />
   unzip("UCI HAR Dataset.zip", exdir = getwd()) <br />
} <br />

# Read and Convert Data <br />
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ') <br />
features <- as.character(features[,2]) <br />

data.train.x <- read.table('./UCI HAR Dataset/train/X_train.txt') <br />
data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ') <br />
data.train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ') <br />
data.train <- data.frame(data.train.subject, data.train.activity, data.train.x) <br />
names(data.train) <- c(c('subject', 'activity'), features) <br />

data.test.x <- read.table('./UCI HAR Dataset/test/X_test.txt') <br />
data.test.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ') <br />
data.test.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ') <br />
data.test <- data.frame(data.test.subject, data.test.activity, data.test.x) <br />
names(data.test) <- c(c('subject', 'activity'), features) <br />

# 1. Merges the Training and Testing Sets into 1 data set called data.all <br />
data.all <- rbind(data.train, data.test) <br />

# 2. Extracts only the measurements on the mean and standard deviation for each measurement <br />
mean_std.select <- grep('mean|std', features) <br />
data.sub <- data.all[,c(1,2,mean_std.select + 2)] <br />

# 3. Uses descriptive activity names to name the activities in the data set <br />
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE) <br />
activity.labels <- as.character(activity.labels[,2]) <br />
data.sub$activity <- activity.labels[data.sub$activity] <br />

# 4. Appropriately labels the data set with descriptive variable names <br />
name.new <- names(data.sub) <br />
name.new <- gsub("[(][)]", "", name.new) <br />
name.new <- gsub("^t", "TimeDomain_", name.new) <br />
name.new <- gsub("^f", "FrequencyDomain_", name.new) <br />
name.new <- gsub("Acc", "Accelerometer", name.new) <br />
name.new <- gsub("Gyro", "Gyroscope", name.new) <br />
name.new <- gsub("Mag", "Magnitude", name.new) <br />
name.new <- gsub("-mean-", "_Mean_", name.new) <br />
name.new <- gsub("-std-", "_StandardDeviation_", name.new) <br />
name.new <- gsub("-", "_", name.new) <br />
names(data.sub) <- name.new <br />

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject. <br />
data.tidy <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean) <br />
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE) <br />
