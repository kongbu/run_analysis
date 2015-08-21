# This R script does the following:

# 1. Merges the training and the test sets to create one data set.

tmpXTrain <- read.table("data/UCI_HAR_Dataset/train/X_train.txt")
tmpXTest <- read.table("data/UCI_HAR_Dataset/test/X_test.txt")
X <- rbind(tmpXTrain, tmpXTest)

tmpSubTrain <- read.table("data/UCI_HAR_Dataset/train/subject_train.txt")
tmpSubTest <- read.table("data/UCI_HAR_Dataset/test/subject_test.txt")
Sub <- rbind(tmpSubTrain, tmpSubTest)

tmpYTrain <- read.table("data/UCI_HAR_Dataset/train/y_train.txt")
tmpYTest <- read.table("data/UCI_HAR_Dataset/test/y_test.txt")
Y <- rbind(tmpYTrain, tmpYTest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("data/UCI_HAR_Dataset/features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices]
names(X) <- features[indices, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("data/UCI_HAR_Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names. 

names(Sub) <- "subject"
cleanData <- cbind(Sub, Y, X)
write.table(cleanData, "clean_data.txt",row.name=FALSE)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(Sub)[,1]
numSubjects = length(unique(Sub)[,1])
numActivities = length(activities[,1])
numCols = dim(cleanData)[2]
result = cleanData[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
        for (a in 1:numActivities) {
                result[row, 1] = uniqueSubjects[s]
                result[row, 2] = activities[a, 2]
                tmp <- cleanData[cleanData$subject==s & cleanData$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "clean_data_with_averages.txt",,row.name=FALSE)
