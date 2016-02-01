##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Calcifer Hawkins

# runAnalysis.r File Description:

# This script performs the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################
# Clean up the workspace
rm(list=ls())


# 1. Merges the training and the test sets to create one data set.

#set working directory to the location where UCI HAR Dataset was unzipped
setwd('D:/R-3.2.3/coursera/UCI HAR Dataset/')

# Reads and stores the Train data from files
features = read.table('./features.txt', header=FALSE) #reads and stores features.txt
activity_labels = read.table('./activity_labels.txt', header=FALSE) #reads and stores activity_labels.txt
train_subject = read.table('./train/subject_train.txt', header=FALSE) #reads and stores subject_train.txt
train_x = read.table('./train/X_train.txt', header=FALSE) #reads and stores X_train.txt
train_y = read.table('./train/y_train.txt', header=FALSE) #reads and stores y_train.txt

# Sets column names to the data stored above
colnames(activity_labels) = c('activityId', 'activity_labels')
colnames(train_subject) = "subjectId"
colnames(train_x) = features[, 2]
colnames(train_y) = "activityId"

# Creates the final train dataset by merging train_subject, train_x and train_y
train_data = cbind(train_subject, train_x, train_y)

# Reads and stores in the test data
test_subject = read.table('./test/subject_test.txt', header=FALSE) #reads and stores subject_test.txt
test_x = read.table('./test/X_test.txt', header=FALSE) #reads and stores X_test.txt
test_y = read.table('./test/y_test.txt', header=FALSE) #reads and stores y_test.txt

# Sets column names to the test data stored above
colnames(test_subject) = "subjectId"
colnames(test_x) = features[,2] 
colnames(test_y) = "activityId"


# Creates the final test dataset by merging the test_subject, test_x and test_y
test_data = cbind(test_subject, test_x, test_y)


# Combines train and test data to create a final dataset
final_data = rbind(train_data, test_data)

# Creates a vector for the column names from the final_data, which will be used
# to select the desired mean() and stddev() columns
col_names = colnames(final_data) 



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Creates a logical vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logical_vector = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names))

# Subsets final_data table based on the logical_vector to keep only desired columns
final_data = final_data[logical_vector == TRUE]


# 3. Uses descriptive activity names to name the activities in the data set
# Merges the final_data set with the acitivity_labels table to include descriptive activity names
final_data = merge(final_data, activity_labels, by = 'activityId', all.x = TRUE)

# Updates the col_names vector to include the new column names after merge
col_names  = colnames(final_data)


# 4. Appropriately labels the data set with descriptive variable names. 
# Cleans up the variable names
for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("^(t)","time",col_names[i])
  col_names[i] = gsub("^(f)","freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
}

# Resets the new descriptive column names to the final_data set
colnames(final_data) = col_names



# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Creates a new table, finalDataNoActivityType without the activity_labels column
finalDataNoActivityType = final_data[,names(final_data) != 'activity_labels']

# Summarizes the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidy_data = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merges the tidy_data with activity_labels to include descriptive acitvity names
tidy_data = merge(tidy_data,activity_labels,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidy_data, './tidy_data.txt',row.names = FALSE,sep='\t')