## This script is for the week 4 assignment of the Getting and Cleaning
## Data course

## Load packages required
## Install dplyr
library(data.table)
library(dplyr)

## Download the raw data file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/fhdataset.zip")

## Unzip the file
unzip("./data/fhdataset.zip")

##read the files into variables trainsdt, trainadt, traindt, testsdt, testadt, testdt
## trainsdt is the subject index for the study observation
## trainadt is the activity index for the study observation
## traindt contains the observation variables
## testsdt is the subject index for the study observation
## testadt is the activity index for the study observation
## testdt contains the observation variables
trainadt <- fread(input="./UCI HAR Dataset/train/y_train.txt")
trainsdt <- fread(input="./UCI HAR Dataset/train/subject_train.txt")
traindt <- fread(input="./UCI HAR Dataset/train/X_train.txt")
testadt <- fread(input="./UCI HAR Dataset/test/y_test.txt")
testsdt <- fread(input="./UCI HAR Dataset/test/subject_test.txt")
testdt <- fread(input="./UCI HAR Dataset/test/X_test.txt")
labeldt <- fread(input="./UCI HAR Dataset/features.txt")
activitylabel <- fread(input="./UCI HAR Dataset/activity_labels.txt")

## assign column names for activities and variables
colnames(activitylabel) <- c("index", "activity")
colnames(labeldt) <- c("index",  "variables")

## The variables in the st and yt file are labeled V1 and the xt file
## also has a V1 variable, so the variables need to be renamed before
## combining the files.
colnames(trainsdt) <- c("subject")
colnames(trainadt) <- c("activity")
colnames(traindt) <- labeldt$variables
colnames(testsdt) <- c("subject")
colnames(testadt) <- c("activity")
colnames(testdt) <- labeldt$variables

## remove the extra column labels and clean up the column labels
features <- grep("(mean|std)\\(\\)", labeldt[,variables])
values <- labeldt[features, variables]
values <- gsub('[()]','', values)
values <- gsub('^f', 'frequency', values)
values <- gsub('^t', 'time', values)
values <- gsub('Acc', 'acceleration', values)
values <- gsub('[-]','', values)
values <- tolower(values)
activitylabel[["activity"]] <- tolower(activitylabel[["activity"]])
activitylabel[["activity"]] <- gsub('_', '', activitylabel[["activity"]])

## remove the extra columns from the data tables
traindt <- traindt[, features, with=FALSE]
testdt <- testdt[, features, with=FALSE]
colnames(traindt) <- values
colnames(testdt) <- values

## column bind the train tables together, and the test tables together
traindt <- cbind(trainsdt, trainadt, traindt)
testdt <- cbind(testsdt, testadt, testdt)

## merge the two data tables
totaldt <- rbind(traindt, testdt)

##set factors and sort
totaldt[["activity"]] <- factor(totaldt[,activity], levels=activitylabel[["index"]], labels = activitylabel[["activity"]])
totaldt[["subject"]] <- as.factor(totaldt[,subject])

## melt and cast
totaldt <- melt(data = totaldt, id = c("subject", "activity"))
totaldt <- dcast(data = totaldt, subject + activity ~ variable, fun.aggregate = mean)

## write the tidy data to a csv file named tidydata.csv
fwrite(x=totaldt, file = "tidydata.csv")