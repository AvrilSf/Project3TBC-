# download and read files

fileUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./Data2/UCI%20HAR%20Dataset.zip", method = "curl")
unzip("./Data2/UCI%20HAR%20Dataset.zip", exdir = "./Data2")
feat <- read.csv("./Data2/UCI_HAR_Dataset/features.txt", sep = "", header = FALSE)
library(dplyr)
library(tidyr)
library(readr)
features <- tbl_df(feat)
colnames(features) <- c("feature_number", "feature_name")
activity_lables <- read.csv("./Data2/UCI_HAR_Dataset/activity_labels.txt", sep = "", header = FALSE)
activity_lables <- tbl_df(activity_lables)
colnames(activity_lables) <- c("activity_number", "activity_label")
subject_train <- read.csv("./Data2/UCI_HAR_Dataset/train/subject_train.txt", sep = "", header = FALSE)
subject_train <- tbl_df(subject_train)
colnames(subject_train) <- c("subject_train")
y_train <- read.csv("./Data2/UCI_HAR_Dataset/train/y_train.txt", sep = "", header = FALSE)
y_train <- tbl_df(y_train)
colnames(y_train) <- c("activity_number")
x_train <- read.csv("./Data2/UCI_HAR_Dataset/train/x_train.txt", sep = "", header = FALSE)
x_train <- tbl_df(x_train)
colnames(x_train) <- features$feature_number
subject_test <- read.csv("./Data2/UCI_HAR_Dataset/test/subject_test.txt", sep = "", header = FALSE)
subject_test <- tbl_df(subject_test)
colnames(subject_test) <- c("subject_test")
y_test <- read.csv("./Data2/UCI_HAR_Dataset/test/y_test.txt", sep = "", header = FALSE)
y_test <- tbl_df(y_test)
colnames(y_test) <- c("activity_number")
x_test <- read.csv("./Data2/UCI_HAR_Dataset/test/x_test.txt", sep = "", header = FALSE)
x_test <- tbl_df(x_test)
colnames(x_test) <- features$feature_number

#merge training and test sets to create one data set
y_train <- mutate(y_train, activity_label = factor(y_train$activity_number, levels = c(1, 2, 3, 4, 5, 6), labels = c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")))
y_test <- mutate(y_test, activity_label = factor(y_test$activity_number, levels = c(1, 2, 3, 4, 5, 6), labels = c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")))
test <- mutate(subject_test, "activity" = y_test$activity_label)
test2 <- mutate(x_test, "subject" = test$subject_test, "activity" = test$activity)
test2 <- select(test2, "subject", "activity", 1:561)
colnames(test2) <- c("subject", "activity", features$feature_name)

train <- mutate(subject_train, "activity" = y_train$activity_label)
train2 <- mutate(x_train, "subject" = train$subject_train, "activity" = train$activity)
train2 <- select(train2, "subject", "activity", 1:561)
colnames(train2) <- c("subject", "activity", features$feature_name)

#extracts only measurements on the mean and standard deviation for each measurement
testntrain <- bind_rows(test2,train2)
meanstd <- select(testntrain, 1:8, 43:48, 83:88,  123:128,163:168, 203:204, 216:217, 229:230, 242:243, 255:256, 268:273, 296:298, 347:352, 375:377, 426:431, 454:456, 505:506, 515, 518:519, 528, 531:532, 541, 544:545, 554, 557:563)
names(meanstd) <- gsub("-", "_", names(meanstd))
names(meanstd) <- gsub("\\)", "", names(meanstd))
names(meanstd) <- gsub("\\(", "", names(meanstd))
names(meanstd) <- gsub("angle", "angle_", names(meanstd))
names(meanstd) <- gsub("\\,", "", names(meanstd))
names(meanstd) <- gsub("BodyBody", "Body", names(meanstd))
View(meanstd)

# from meanstd creates a  second, independent tidy data set with the average of each variable for each activity and each subject.
avg_data <- meanstd %>% group_by(activity, subject) %>% summarize(avg_tBodyAcc_mean_X = mean(tBodyAcc_mean_X), avg_tBodyAcc_mean_Y = mean(tBodyAcc_mean_Y), avg_tBodyAcc_mean_Z = mean(tBodyAcc_mean_Z), avg_tBodyAcc_std_X = mean(tBodyAcc_std_X), avg_tBodyAcc_std_Y = mean(tBodyAcc_std_Y), avg_tBodyAcc_std_Z = mean(tBodyAcc_std_Z), avg_tGravityAcc_mean_X = mean(tGravityAcc_mean_X), avg_tGravityAcc_mean_Y = mean(tGravityAcc_mean_Y), avg_tGravityAcc_mean_Z = mean(tGravityAcc_mean_Z), avg_tGravityAcc_std_X = mean(tGravityAcc_std_X), avg_tGravityAcc_std_Y = mean(tGravityAcc_std_Y), avg_tGravityAcc_std_Z = mean(tGravityAcc_std_Z), avg_tBodyAccJerk_mean_X = mean(tBodyAccJerk_mean_X), avg_tBodyAccJerk_mean_Y = mean(tBodyAccJerk_mean_Y), avg_tBodyAccJerk_mean_Z = mean(tBodyAccJerk_mean_Z), avg_tBodyAccJerk_std_X = mean(tBodyAccJerk_std_X), avg_tBodyAccJerk_std_Y = mean(tBodyAccJerk_std_Y), avg_tBodyAccJerk_std_Z = mean(tBodyAccJerk_std_Z), avg_tBodyGyro_mean_X = mean(tBodyGyro_mean_X), avg_tBodyGyro_mean_Y = mean(tBodyGyro_mean_Y), avg_tBodyGyro_mean_Z = mean(tBodyGyro_mean_Z), avg_tBodyGyro_std_X = mean(tBodyGyro_std_X), avg_tBodyGyro_std_Y = mean(tBodyGyro_std_Y), avg_tBodyGyro_std_Z = mean(tBodyGyro_std_Z), avg_tBodyGyroJerk_mean_X = mean(tBodyGyroJerk_mean_X), avg_tBodyGyroJerk_mean_Y = mean(tBodyGyroJerk_mean_Y), avg_tBodyGyroJerk_mean_Z = mean(tBodyGyroJerk_mean_Z), avg_tBodyGyroJerk_std_X = mean(tBodyGyroJerk_std_X), avg_tBodyGyroJerk_std_Y = mean(tBodyGyroJerk_std_Y), avg_tBodyGyroJerk_std_Z = mean(tBodyGyroJerk_std_Z), avg_tBodyAccMag_mean = mean(tBodyAccMag_mean), avg_tBodyAccMag_std = mean(tBodyAccMag_std), avg_tGravityAccMag_mean = mean(tGravityAccMag_mean), avg_tGravityAccMag_std = mean(tGravityAccMag_std), avg_tBodyAccJerkMag_mean = mean(tBodyAccJerkMag_mean), avg_tBodyAccJerkMag_std = mean(tBodyAccJerkMag_std) ,avg_tBodyGyroMag_mean = mean(tBodyGyroMag_mean), avg_tBodyGyroMag_std = mean(tBodyGyroMag_std), avg_tBodyGyroJerkMag_mean = mean(tBodyGyroJerkMag_mean), avg_tBodyGyroJerkMag_std = mean(tBodyGyroJerkMag_std), avg_fBodyAcc_mean_X = mean(fBodyAcc_mean_X), avg_fBodyAcc_mean_Y = mean(fBodyAcc_mean_Y), avg_fBodyAcc_mean_Z = mean(fBodyAcc_mean_Z), avg_fBodyAcc_std_X = mean(fBodyAcc_std_X), avg_fBodyAcc_std_Y = mean(fBodyAcc_std_Y), avg_fBodyAcc_std_Z = mean(fBodyAcc_std_Z), avg_fBodyAccJerk_mean_X = mean(fBodyAccJerk_mean_X), avg_fBodyAccJerk_mean_Y = mean(fBodyAccJerk_mean_Y), avg_fBodyAccJerk_mean_Z = mean(fBodyAccJerk_mean_Z), avg_fBodyAccJerk_std_X = mean(fBodyAccJerk_std_X), avg_fBodyAccJerk_std_Y = mean(fBodyAccJerk_std_Y), avg_fBodyAccJerk_std_Z = mean(fBodyAccJerk_std_Z), avg_fBodyGyro_mean_X = mean(fBodyGyro_mean_X), avg_fBodyGyro_mean_Y = mean(fBodyGyro_mean_Y), avg_fBodyGyro_mean_Z = mean(fBodyGyro_mean_Z), avg_fBodyGyro_std_X = mean(fBodyGyro_std_X), avg_fBodyGyro_std_Y = mean(fBodyGyro_std_Y), avg_fBodyGyro_std_Z = mean(fBodyGyro_std_Z), avg_fBodyAccMag_mean = mean(fBodyAccMag_mean), avg_fBodyAccMag_std = mean(fBodyAccMag_std), avg_fBodyAccMag_meanFreq = mean(fBodyAccMag_meanFreq), avg_fBodyAccJerkMag_mean = mean(fBodyAccJerkMag_mean), avg_fBodyAccJerkMag_std = mean(fBodyAccMag_std), avg_fBodyAccJerkMag_meanFreq = mean(fBodyAccJerkMag_meanFreq), avg_fBodyGyroMag_mean = mean(fBodyGyroMag_mean), avg_fBodyGyroMag_std = mean(fBodyGyroMag_std), avg_fBodyGyroJerkMag_mean = mean(fBodyGyroJerkMag_mean), avg_fBodyGyroJerkMag_std = mean(fBodyGyroJerkMag_std), avg_angle_tBodyAccMeangravity = mean(angle_tBodyAccMeangravity), avg_angle_tBodyAccJerkMeangravityMean = mean(angle_tBodyAccJerkMeangravityMean), avg_angle_tBodyGyroJerkMeangravityMean = mean(angle_tBodyGyroMeangravityMean), avg_angle_tBodyGyroJerkMeangravityMean = mean(angle_tBodyGyroJerkMeangravityMean), avg_angle_XgravityMean = mean(angle_XgravityMean), avg_angle_YgravityMean = mean(angle_YgravityMean), avg_angle_ZgravityMean = mean(angle_ZgravityMean)) 
View(avg_data)