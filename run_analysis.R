### 0. Loading files
# Creating a new directory
if(!file.exists("./data")){dir.create("./data")}
# Download the .zip
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip")
# Unzip DataSet to /data directory
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")
# Define the path where the new folder has been unziped
pathdata = file.path("./data", "UCI HAR Dataset")
# Create a file which has the 28 file names
files = list.files(pathdata, recursive=TRUE)
# Show the files
files

### 1. Making the Test and Training Set Data
# Reading training tables - xtrain / ytrain, subject_train
xtrain = read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"),
                           header = FALSE)
# Checking
str(xtrain)
str(ytrain)
str(subject_train)
# Reading the testing tables - xtest / ytest, subject_test
xtest = read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(pathdata, "test", "subject_test.txt"),
                          header = FALSE)
# Checking
str(xtest)
str(ytest)
str(subject_test)
# Read the features data
features = read.table(file.path(pathdata, "features.txt"),header = FALSE)
# Checking
str(features)
# Read activity labels data
activityLabels = read.table(file.path(pathdata, "activity_labels.txt"),
                            header = FALSE)
# Checking
str(activityLabels)
# Create the values corresponding to each column to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityID"
colnames(subject_train) = "subjectID"
# Create the values corresponding to each column to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityID"
colnames(subject_test) = "subjectID"
# Create a variable for the check value of the activity labels
colnames(activityLabels) <- c('activityID','activityType')
# Merging the train and test data
data_train = cbind(ytrain, subject_train, xtrain)
data_test = cbind(ytest, subject_test, xtest)
# Create the main data table merging both table tables
data_main = rbind(data_train, data_test)

### 2. Extracting only the measurements on the mean and standard deviation for 
###    each measurement
# Extract the columns and rows that match the search
mean_and_std = data_main %>% select(subjectID, activityID, contains("mean"), contains("std"))

### 3. Uses descriptive activity names to name the activities in the data set
# Merge the mean and standard deviation dataset with the activity labels
data_Merge = merge(mean_and_std, activityLabels, by="activityID", all.x=TRUE)

### 4. Appropriately labels the data set with descriptive variable names.
# Correct the name of each column
# Eliminate extra variable
data_Merge$activityID <- NULL
names(data_Merge) = gsub("Acc", "Accelerometer", names(data_Merge))
names(data_Merge) = gsub("Gyro", "Gyroscope", names(data_Merge))
names(data_Merge) = gsub("BodyBody", "Body", names(data_Merge))
names(data_Merge) = gsub("Mag", "Magnitude", names(data_Merge))
names(data_Merge) = gsub("^t", "Time", names(data_Merge))
names(data_Merge) = gsub("^f", "Frequency", names(data_Merge))
names(data_Merge) = gsub("tBody", "TimeBody", names(data_Merge))
names(data_Merge) = gsub("-mean()", "Mean", names(data_Merge), 
                        ignore.case = TRUE)
names(data_Merge) = gsub("-std()", "STD", names(data_Merge), 
                        ignore.case = TRUE)
names(data_Merge) = gsub("-freq()", "Frequency", names(data_Merge), 
                        ignore.case = TRUE)
names(data_Merge) = gsub("angle", "Angle", names(data_Merge))
names(data_Merge) = gsub("gravity", "Gravity", names(data_Merge))

### 5. From the data set in step 4, creates a second, independent tidy data set 
### with the average of each variable for each activity and each subject.
# create the summary taking the means of each variable for each activity and 
# each subject, after groupped by subject and activity
FinalData <- data_Merge %>%
        group_by(subjectID, activityType) %>%
        summarise_all(funs(mean))
# Checking
str(FinalData)
# View
FinalData
# Writing the result and export the text file
write.table(FinalData, "./FinalData.txt", row.name=FALSE)