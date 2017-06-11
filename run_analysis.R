run_analysis <- function() {
##
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


setwd("c:/coursera/datascience/GettingandCleaningData/UCI HAR Dataset/UCI HAR Dataset")

library(data.table)
  

# load training data
subject_train = read.table("train/subject_train.txt", col.names=c("subject"))
X_train = read.table("train/X_train.txt")
Y_train = read.table("train/Y_train.txt",col.names = c("activityID"))
  
# load test data  
subject_test = read.table("test/subject_test.txt", col.names=c("subject"))
X_test = read.table("test/X_test.txt")
Y_test = read.table("test/Y_test.txt", col.names = c("activityID"))

#load features and activities
features = read.table("features.txt", 
                      sep=" ", 
                      col.names=c("featureID", "featureName"), 
                      fill=FALSE, strip.white=TRUE)

activity = read.table("activity_labels.txt", 
                      sep=" ", 
                      col.names=c("activityID", "activityName")) 

#Add descriptive names to activity
activity$activityName <- gsub("_"," ",tolower(activity$activityName))

#combine data

y_data <- rbind(Y_train,Y_test)
x_data <- rbind(X_train,X_test)
all_data <- cbind(x_data,y_data,subject)

#transpose shape features, rows into columns
colnames(x_data) <- t(features$featureName)

#features to include

mean_std <- grep("-mean\\(|-std\\(", names(all_data), ignore.case=TRUE)
required_columns <- c(mean_std,562,563)
required_data <- all_data[,required_columns]
tidy_data <- merge(required_data,activity, by ="activityID")

#write tidy data 
write.table(tidy_data, "tidy_data.txt")


# create a dataset grouped by subject and activity after applying standard deviation and average calculations

DT <- data.table(tidy_data)
calculated_data<- DT[, lapply(.SD, mean), by=c("subject", "activityName")]

write.table(calculated_data ,"calculated_data.txt")
}

