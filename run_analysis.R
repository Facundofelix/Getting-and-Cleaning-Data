library(dplyr)
library(utils)

# STEP 1 
# Merges the training and the test sets to create one data set.
project <- "Coursera.Project.zip"

if (!file.exists(project)){
  projecturl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(projecturl, project, method = "curl")
}

if (!file.exists("UCI HAR Dataset")){ unzip(project)}

## We have to list the different data sets

features <- read.table("UCI HAR Dataset/features.txt", header = FALSE,
                       col.names = c("N°","features"))

activity <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE,
                       col.names = c("Code","Activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE,
                           col.names = "Subject")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE,
                           col.names = "Subject")


set_train <-  read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE,
                            col.names = features$features)

set_test <-  read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE,
                         col.names = features$features)

labels_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE,
                           col.names = "Code")

labels_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE,
                           col.names = "Code")


## We have to merge data. Firstly, we merge Activity with labels_train and labels_test


labels <- rbind(labels_train, labels_test)

set <- rbind(set_train, set_test)

Subject <- rbind(subject_train, subject_test)

complete_list <- cbind(Subject, labels, set)


# STEP 2 
# Extracts only the measurements on the mean 
# and standard deviation for each measurement.

library(tidyselect)
new_data <- select(complete_list, Subject:Code,
                   contains("mean"),
                   contains("std"))


# STEP 3
# Uses descriptive activity names to name the 
# activities in the data set

str(activity)
str(features)

new_data$Code <- activity[new_data$Code,2]
 
# STEP 4
# Appropriately labels the data set 
# with descriptive variable names.


names(new_data) <- gsub("Code", "Activity", names(new_data))
names(new_data) <- gsub("Acc", " Accelerometer", names(new_data))
names(new_data) <- gsub("tBody", "Time Body ", names(new_data))
names(new_data) <- gsub("^t", "Time ", names(new_data))
names(new_data) <- gsub("[Gg]yro", "Gyroscope ", names(new_data))
names(new_data) <- gsub("^f", "Frecuency ", names(new_data))
names(new_data) <- gsub("[Mm]ag", "Magnitude", names(new_data))
names(new_data) <- gsub("BodyBody", "Body", names(new_data))
names(new_data) <- gsub("angle", "Angle", names(new_data), ignore.case = TRUE)
names(new_data) <- gsub("gravity", "Gravity", names(new_data), ignore.case = TRUE)
names(new_data) <- gsub("mean", "Mean", names(new_data))
names(new_data) <- gsub("[Ff]req", "Frequency", names(new_data))
names(new_data) <- gsub("[Ss]td", "STD", names(new_data))
names(new_data) <- gsub("Jerk", " Jerk", names(new_data))
names(new_data) <- gsub("...X", " of X", names(new_data))
names(new_data) <- gsub("...Y", " of Y", names(new_data))
names(new_data) <- gsub("...Z", " of Z", names(new_data))


#STEP 5
# From the data set in step 4, creates a second, 
# independent tidy data set with the average of 
# each variable for each activity and each subject.

independent_data <- new_data %>%
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))

independent_data

  

