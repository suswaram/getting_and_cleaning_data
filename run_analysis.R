# merge the training and the test data sets

temp1 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/train/X_train.txt")
temp2 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/test/X_test.txt")
X <- rbind(temp1, temp2)

temp1 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/train/subject_train.txt")
temp2 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/test/subject_test.txt")
S <- rbind(temp1, temp2)

temp1 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/train/y_train.txt")
temp2 <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/test/y_test.txt")
Y <- rbind(temp1, temp2)

# extract the measurements on the mean and standard deviation

features <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

#use descriptive activity names to name the activity_labels in the data set

activity_labels <- read.table("/Users/HariniSuswaram1/UCI_HAR_Dataset/activity_labels.txt")
activity_labels[, 2] = gsub("_", "", tolower(as.character(activity_labels[, 2])))
Y[,1] = activity_labels[Y[,1], 2]
names(Y) <- "activity"

#label the data set appropriately with descriptive activity names.

names(S) <- "subject"
clean <- cbind(S, Y, X)
write.table(clean, "merged_clean_data.txt")

#create a new independent tidy data set with the average of each variable for each activity and each subject.

uniquesubs = unique(S)[,1]
numsubs = length(unique(S)[,1])
numactivity_labels = length(activity_labels[,1])
numcols = dim(clean)[2]
res = clean[1:(numsubs*numactivity_labels), ]

row = 1
for (s in 1:numsubs) {
  for (a in 1:numactivity_labels) {
    res[row, 1] = uniquesubs[s]
    res[row, 2] = activity_labels[a, 2]
    temp <- clean[clean$subject==s & clean$activity==activity_labels[a, 2], ]
    res[row, 3:numcols] <- colMeans(temp[, 3:numcols])
    row = row+1
  }
}
write.table(res, "averages.txt")

