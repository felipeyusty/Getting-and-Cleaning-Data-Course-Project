Codebook
========

Variables
---------

dt: FinalData

Variable                                      | Comments
----------------------------------------------|------------------------
SubjectID                                     |  subject identifier of volunteer (1-30)
ActivityType                                  |  name ofactivity subject performed (LAYING,SITTING,STANDING,WALKING,WALKING_DOWNSTAIRS,WALKING_UPSTAIRS)
TimeBodyAccelerometerMean()-X                 |  mean of tBodyAcc-mean()-X
TimeBodyAccelerometerMean()-Y                 |  mean of tBodyAcc-mean()-Y
TimeBodyAccelerometerMean()-Z                 |  mean of tBodyAcc-mean()-Z
TimeBodyAccelerometerSTD()-X                  |  mean of tBodyAcc-std()-X
TimeBodyAccelerometerSTD()-Y                  |  mean of tBodyAcc-std()-Y
TimeBodyAccelerometerSTD()-Z                  |  mean of tBodyAcc-std()-Z
TimeGravityAccelerometerMean()-X              |  mean of tGravityAcc-mean()-X
TimeGravityAccelerometerMean()-Y              |  mean of tGravityAcc-mean()-Y
TimeGravityAccelerometerMean()-Z              |  mean of tGravityAcc-mean()-Z
TimeGravityAccelerometerSTD()-X               |  mean of tGravityAcc-std()-X
TimeGravityAccelerometerSTD()-Y               |  mean of tGravityAcc-std()-Y
TimeGravityAccelerometerSTD()-Z               |  mean of tGravityAcc-std()-Z
TimeBodyAccelerometerJerkMean()-X             |  mean of tBodyAccJerk-mean()-X
TimeBodyAccelerometerJerkMean()-Y             |  mean of tBodyAccJerk-mean()-Y
TimeBodyAccelerometerJerkMean()-Z             |  mean of tBodyAccJerk-mean()-Z
TimeBodyAccelerometerJerkSTD()-X              |  mean of tBodyAccJerk-std()-X
TimeBodyAccelerometerJerkSTD()-Y              |  mean of tBodyAccJerk-std()-Y
TimeBodyAccelerometerJerkSTD()-Z              |  mean of tBodyAccJerk-std()-Z
TimeBodyGyroscopeMean()-X                     |  mean of tBodyGyro-mean()-X
TimeBodyGyroscopeMean()-Y                     |  mean of tBodyGyro-mean()-Y
TimeBodyGyroscopeMean()-Z                     |  mean of tBodyGyro-mean()-Z
TimeBodyGyroscopeSTD()-X                      |  mean of tBodyGyro-std()-X
TimeBodyGyroscopeSTD()-Y                      |  mean of tBodyGyro-std()-Y
TimeBodyGyroscopeSTD()-Z                      |  mean of tBodyGyro-std()-Z
TimeBodyGyroscopeJerkMean()-X                 |  mean of tBodyGyroJerk-mean()-X
TimeBodyGyroscopeJerkMean()-Y                 |  mean of tBodyGyroJerk-mean()-Y
TimeBodyGyroscopeJerkMean()-Z                 |  mean of tBodyGyroJerk-mean()-Z
TimeBodyGyroscopeJerkSTD()-X                  |  mean of tBodyGyroJerk-std()-X
TimeBodyGyroscopeJerkSTD()-Y                  |  mean of tBodyGyroJerk-std()-Y
TimeBodyGyroscopeJerkSTD()-Z                  |  mean of tBodyGyroJerk-std()-Z
TimeBodyAccelerometerMagnitudeMean()          |  mean of tBodyAccMag-mean()
TimeBodyAccelerometerMagnitudeSTD()           |  mean of tBodyAccMag-std()
TimeGravityAccelerometerMagnitudeMean()       |  mean of tGravityAccMag-mean()
TimeGravityAccelerometerMagnitudeSTD()        |  mean of tGravityAccMag-std()
TimeBodyAccelerometerJerkMagnitudeMean()      |  mean of tBodyAccJerkMag-mean()
TimeBodyAccelerometerJerkMagnitudeSTD()       |  mean of tBodyAccJerkMag-std()
TimeBodyGyroscopeMagnitudeMean()              |  mean of tBodyGyroMag-mean()
TimeBodyGyroscopeMagnitudeSTD()               |  mean of tBodyGyroMag-std()
TimeBodyGyroscopeJerkMagnitudeMean()          |  mean of tBodyGyroJerkMag-mean()
TimeBodyGyroscopeJerkMagnitudeSTD()           |  mean of tBodyGyroJerkMag-std()
FrequencyBodyAccelerometerMean()-X            |  mean of fBodyAcc-mean()-X
FrequencyBodyAccelerometerMean()-Y            |  mean of fBodyAcc-mean()-Y
FrequencyBodyAccelerometerMean()-Z            |  mean of fBodyAcc-mean()-Z
FrequencyBodyAccelerometerSTD()-X             |  mean of fBodyAcc-std()-X
FrequencyBodyAccelerometerSTD()-Y             |  mean of fBodyAcc-std()-Y
FrequencyBodyAccelerometerSTD()-Z             |  mean of fBodyAcc-std()-Z
FrequencyBodyAccelerometerMeanFreq()-X        |  mean of fBodyAccJerk-mean()-X
FrequencyBodyAccelerometerMeanFreq()-Y        |  mean of fBodyAccJerk-mean()-Y
FrequencyBodyAccelerometerMeanFreq()-Z        |  mean of fBodyAccJerk-mean()-Z
FrequencyBodyAccelerometerJerkSTD()-X         |  mean of fBodyAccJerk-std()-X
FrequencyBodyAccelerometerJerkSTD()-Y         |  mean of fBodyAccJerk-std()-Y
FrequencyBodyAccelerometerJerkSTD()-Z         |  mean of fBodyAccJerk-std()-Z
FrequencyBodyGyroscopeMean()-X                |  mean of fBodyGyro-mean()-X
FrequencyBodyGyroscopeMean()-Y                |  mean of fBodyGyro-mean()-Y
FrequencyBodyGyroscopeMean()-Z                |  mean of fBodyGyro-mean()-Z
FrequencyBodyGyroscopeSTD()-X                 |  mean of fBodyGyro-std()-X
FrequencyBodyGyroscopeSTD()-Y                 |  mean of fBodyGyro-std()-Y
FrequencyBodyGyroscopeSTD()-Z                 |  mean of fBodyGyro-std()-Z
FrequencyBodyAccelerometerMagnitudeMean()     |  mean of fBodyAccMag-mean()
FrequencyBodyAccelerometerMagnitudeSTD()      |  mean of fBodyAccMag-std()
FrequencyBodyAccelerometerJerkMagnitudeMean() |  mean of fBodyBodyAccJerkMag-mean()
FrequencyBodyAccelerometerJerkMagnitudeSTD()  |  mean of fBodyBodyAccJerkMag-std()
FrequencyBodyGyroscopeMagnitudeMean()         |  mean of fBodyBodyGyroMag-mean()
FrequencyBodyGyroscopeMagnitudeSTD()          |  mean of fBodyBodyGyroMag-std()
FrequencyBodyGyroscopeJerkMagnitudeMean()     |  mean of fBodyBodyGyroJerkMag-mean()
FrequencyBodyGyroscopeJerkMagnitudeSTD()      |  mean of fBodyBodyGyroJerkMag-std()


The `run_analysis.R` script performs the data preparation and then followed by the 5 steps required as described in the course project's definition.

Transformations
---------------

1. Download the dataset
    - Dataset downloaded and extracted under the folder called UCI HAR Dataset

2. Assign each data to variables
    - `features <- features.txt` : 561 rows, 2 columns
          The features selected for this database come from the accelerometer and gyroscope 3-axial raw            signals tAcc-XYZ and tGyro-XYZ.
    - `activityLabels <- activity_labels.txt` : 6 rows, 2 columns
          List of activities performed when the corresponding measurements were taken and its codes                (labels)
    - `subject_test <- test/subject_test.txt` : 2947 rows, 1 column
          contains test data of 9/30 volunteer test subjects being observed
    - `xtest <- test/X_test.txt` : 2947 rows, 561 columns
          contains recorded features test data
    - `ytest <- test/y_test.txt` : 2947 rows, 1 columns
          contains test data of activities'code labels
    - `subject_train <- test/subject_train.txt` : 7352 rows, 1 column
          contains train data of 21/30 volunteer subjects being observed
    - `xtrain <- test/X_train.txt` : 7352 rows, 561 columns
          contains recorded features train data
    - `ytrain <- test/y_train.txt` : 7352 rows, 1 columns
          contains train data of activities'code labels

3. Merges the training and the test sets to create one data set
    - `data_train` (7352 rows, 563 columns) is created by merging `ytrain`, `subject_train` and `xtrain`               using `cbind()` function
    - `data_test` (2947 rows, 563 column) is created by merging `ytest`, `subject_test` and `xtest` using              `cbind()` function
    - `data_main` (10299 rows, 563 column) is created by merging Subject, `data_train` and `data_test`               using `rbind()` function

4. Extracts only the measurements on the mean and standard deviation for each measurement
    - `mean_and_std` (10299 rows, 88 columns) is created by subsetting `data_main`, selecting only columns: `subjectID`, `actividyID` and the measurements on the mean and standard deviation (std) for each measurement.

5. Uses descriptive activity names to name the activities in the data set
    - Entire numbers in `activityID` column of the `mean_and_std` replaced with corresponding `activityLabels` taken from second column of the activities variable and a new database called `data_merge` 

6. Appropriately labels the data set with descriptive variable names
    - `activityID` is eliminate
    - All `Acc` in column's name replaced by `Accelerometer`
    - All `Gyro` in column's name replaced by `Gyroscope`
    - All `BodyBody` in column's name replaced by `Body`
    - All `Mag` in column's name replaced by `Magnitude`
    - All start with character `f` in column's name replaced by `Frequency`
    - All start with character `t` in column's name replaced by `Time`

7. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
    - `FinalData` (180 rows, 88 columns) is created by sumarizing `data_merge` taking the means of each variable for each activity and each subject, after groupped by subject and activity.
    - Export `FinalData` into `FinalData.txt` file.

Data
----

Copied from README.txt in original dataset.

> ==================================================================
> Human Activity Recognition Using Smartphones Dataset
> Version 1.0
> ==================================================================
> Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
> Smartlab - Non Linear Complex Systems Laboratory
> DITEN - Universit� degli Studi di Genova.
> Via Opera Pia 11A, I-16145, Genoa, Italy.
> activityrecognition@smartlab.ws
> www.smartlab.ws
> ==================================================================
> 
> The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
> 
> The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
> 
> For each record it is provided:
> ======================================
> 
> - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
> - Triaxial Angular velocity from the gyroscope. 
> - A 561-feature vector with time and frequency domain variables. 
> - Its activity label. 
> - An identifier of the subject who carried out the experiment.
> 
> The dataset includes the following files:
> =========================================
> 
> - 'README.txt'
> 
> - 'features_info.txt': Shows information about the variables used on the feature vector.
> 
> - 'features.txt': List of all features.
> 
> - 'activity_labels.txt': Links the class labels with their activity name.
> 
> - 'train/X_train.txt': Training set.
> 
> - 'train/y_train.txt': Training labels.
> 
> - 'test/X_test.txt': Test set.
> 
> - 'test/y_test.txt': Test labels.
> 
> The following files are available for the train and test data. Their descriptions are equivalent. 
> 
> - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
> 
> - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
> 
> - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
> 
> - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 
> 
> Notes: 
> ======
> - Features are normalized and bounded within [-1,1].
> - Each feature vector is a row on the text file.
> 
> For more information about this dataset contact: activityrecognition@smartlab.ws
> 
> License:
> ========
> Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
> 
> [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
> 
> This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
> 
> Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
> 
