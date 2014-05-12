{\rtf1\ansi\ansicpg1252\cocoartf1265\cocoasubrtf190
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;\red38\green38\blue38;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\sa300

\f0\fs30 \cf2 the file run_analysis.R takes the data from the UCIHAR Dataset and processes it (details below). The file needs to changed such that the "yourPath" variable specifices where the UCIHAR Dataset variables have been downloaded and uncompressed to.\
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. \
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.\
The dataset includes the following files:\
- 'README.txt'\
- 'features_info.txt': Shows information about the variables used on the feature vector.\
- 'features.txt': List of all features.\
- 'activity_labels.txt': Links the class labels with their activity name.\
- 'train/X_train.txt': Training set.\
- 'train/y_train.txt': Training labels.\
- 'test/X_test.txt': Test set.\
- 'test/y_test.txt': Test labels.\
\
run_analysis.R works by reading in the files from test and training sets, loading in variable names and appropriately name the R variables' columns and rows. The Test and Train datasets are merged into a single matrix. Then mean and standard deviation on the measures are extracted followed by assigning activity names to the activity labels. Later a new independent tidied dataset is created wit the average for each variable for each activity and subject resulting in averages.txt file. \
}