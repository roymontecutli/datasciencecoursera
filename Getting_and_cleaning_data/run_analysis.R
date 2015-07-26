#  Getting and cleaning data. 
## Course project

print("0. Preparation for execution of script.")

# To avoid hardcoded values in the logic of the application, they are all set in the following section.
# If any modification for the execution of the application is required, most likely will be here.
# Definition the folders to hold input data and output results
workingDirectory <- "Getting_and_cleaning_data/data"
resultsDirectory <- "Getting_and_cleaning_data/results"

dataFile <- "https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI HAR Dataset.zip"

zipFile <- paste(workingDirectory,"/UCI-HAR-dataset.zip", sep = "")
fileFeature <- paste(workingDirectory,"/UCI HAR Dataset/features.txt", sep = "")
fileXTest <- paste(workingDirectory,"/UCI HAR Dataset/test/X_test.txt", sep = "")
fileXTrain <- paste(workingDirectory,"/UCI HAR Dataset/train/X_train.txt", sep = "")
fileYTest <- paste(workingDirectory,"/UCI HAR Dataset/test/y_test.txt", sep = "")
fileYTrain <- paste(workingDirectory,"/UCI HAR Dataset/train/y_train.txt", sep = "")
fileLabels <- paste(workingDirectory,"/UCI HAR Dataset/activity_labels.txt", sep = "")
fileSubjectTest <- paste(workingDirectory,"/UCI HAR Dataset/test/subject_test.txt", sep = "")
fileSubjectTrain <- paste(workingDirectory,"/UCI HAR Dataset/train/subject_train.txt", sep = "")
fileResultMean <- paste(resultsDirectory,'/resultMean.txt', sep = "")
fileResultAvg <- paste(resultsDirectory,'/resultAvg.txt', sep = "")
print("   0.1 Variables declared ...")

# If the directories have not been created yet, they will be created
inputData <- file.info(workingDirectory)
if (is.na(inputData) == TRUE || (as.logical(inputData$isdir) == FALSE)) {
      dir.create(workingDirectory)
      print(paste("   0.a", workingDirectory, "directory created ..."))
}
inputData <- file.info(resultsDirectory)
if (is.na(inputData) == TRUE || (as.logical(inputData$isdir) == FALSE)) {
      dir.create(resultsDirectory)
      print(paste("   0.b", resultsDirectory, "directory created ..."))
}

# If the file has not been downloaded yet, it will be retrieved
inputData <- file.info(zipFile)
if (is.na(inputData) == TRUE || inputData$size <= 0) {
      print(paste("   0.c Downloading", dataFile, "..."))
      download.file(url = dataFile, destfile = zipFile, method="auto")
      print(paste("   0.d Decompressing", zipFile, "..."))
      unzip(zipfile = zipFile, exdir = workingDirectory)
}

# 1. Merges the training and the test sets to create one data set.
#valuesTest <- retrieveData("test")
#valuesTrain <- retrieveData("train")

features <- read.table(fileFeature)
valueXTest <- read.table(fileXTest, col.names=features[,2])
valueXTrain <- read.table(fileXTrain, col.names=features[,2])
X <- rbind(valueXTest, valueXTrain)
print("1. Merges the training and the test sets to create one data set.")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
interestingFeatures <- features[grep("(mean|std)\\(", features[,2]),]
meanStd <- X[,interestingFeatures[,1]]
print("2. Extracts only the measurements on the mean and standard deviation for each measurement.")

# 3. Uses descriptive activity names to name the activities in the data set
valueYTest <- read.table(fileYTest, col.names = c('activity'))
valueYTrain <- read.table(fileYTrain, col.names = c('activity'))
y <- rbind(valueYTest, valueYTrain)

labels <- read.table(fileLabels)
for (i in 1:nrow(labels)) {
      code <- as.numeric(labels[i, 1])
      name <- as.character(labels[i, 2])
      y[y$activity == code, ] <- name
}
print("3. Uses descriptive activity names to name the activities in the data set")

# 4. Appropriately labels the data set with descriptive activity names. 
valueXLabels <- cbind(y, X)
meanStdLabels <- cbind(y, meanStd)
write.csv(meanStdLabels, file=fileResultMean, row.names=FALSE)
print("4. Appropriately labels the data set with descriptive activity names.")
print(paste("   4.1 The results are saved in ", fileResultMean))

# 5. Creates a second, independent tidy data set with the average of each variable 
#    for each activity and each subject. 
subjectTest <- read.table(fileSubjectTest, col.names = c('subject'))
subjectTrain <- read.table(fileSubjectTrain, col.names = c('subject'))
subject <- rbind(subjectTest, subjectTrain)
averages <- aggregate(X, by = list(activity = y[,1], subject = subject[,1]), mean)

write.csv(averages, file=fileResultAvg, row.names=FALSE)
print("5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.")
print(paste("   5.1 The results are saved in ", fileResultAvg))
