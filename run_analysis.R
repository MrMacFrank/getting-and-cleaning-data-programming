#Coursera Getting & Cleaning Data
#Programming Course Assignment
#MrMacFrank, on 2015-04-26


# Tasks:
# 1.  Merges the training and the test sets to create one data set.
# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.  Uses descriptive activity names to name the activities in the data set
# 4.  Appropriately labels the data set with descriptive activity names. 
# 5.  Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# clean up workspace & get working directory
rm(list=ls())
getwd()

# 0 download ZIP file (if not already downloaded!)
zip <- "UCI HAR Dataset.zip"
fURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url=fURL, destfile=paste(getwd(),"data",zip, sep="/"))

# Task 1:
# Merge datasets after loading data. (only do that, if data has not already been saved
# in the wd. Else, just load the R-Data-File)
if (!file.exists("data.all.RData")){
  data.test<-read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE) #read test data
  data.train<-read.table("UCI HAR Dataset/train/X_train.txt",header=FALSE) #read training data
  
  data.test.subject <- scan("UCI HAR Dataset/test/subject_test.txt") #read test subjects
  data.train.subject <- scan("UCI HAR Dataset/train/subject_train.txt") #read training subjects 
  
  str(data.test.subject)
  
  data.test.activity <- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE) #read test labels
  data.train.activity <- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE) #read training labels
  
  data.values <- read.table("UCI HAR Dataset/features.txt",header=FALSE,stringsAsFactors=FALSE) #reads the names of the variables
  data.labels.activities <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE) #reads the names of the activities
  
  # save "raw data"
  save(list=ls(all=TRUE), file="data.all.RData")
} else {
  load("data.all.RData") # load already saved data file
}

# merge data.test & .train files to one
data.all <- rbind(data.test, data.train)

#name variables according to README.txt file of dataset
str(data.values)
names(data.all)<- data.values$V2

# Task 2:
# Extract mean and standard deviation (SD) for each measurement 

# use only variables that represent means or SD to make a new data frame 
stringmatch <- c("-mean()", "-std()") # indicator for strings that have to be part of the variable names
variablestouse <- data.values$V2[grep(x=data.values$V2, pattern=paste(stringmatch, collapse="|"), perl=FALSE,fixed=FALSE)]

# Exclude "FreqMean"-variables (optional)
# un-comment the following line, if they should be excluded!
# variablestouse <- variablestouse[-grep(variablestouse, pattern="Freq")] # exclude "FreqMean"-variables

# use only means & SD variables in "data.onlyMeans"-df
data.onlyMeans <- data.all[,variablestouse]
str(data.onlyMeans)

# Task 3:
# Use descriptive activity names to label the data set
# & add subject + activity from training + test together
subjects <- c(data.test.subject, data.train.subject)
activity <- rbind(data.test.activity, data.train.activity)
head(activity)

# data now with subjects and activities 
data.full <- data.frame(cbind(subjects, activity, data.onlyMeans))
head(data.full)[1:3]


# make factors out of activities and subjects and label activities
data.full$V1 <- factor(data.full$V1, labels = data.labels.activities$V2)
data.full$subjects <- factor(data.full$subjects, labels = 1:30)

head(data.full)[1:3]
table(data.full$subject) # see summary of subjects


# Task 4:
# didn't understand instruction. The variables - to my mind - were labelled following their
# meaning. only the use of upper and lower cases, in combination to "()-" could be
# improved. Therefore I cleaned the set of upper case letters.
# give meaningful variable names
variablestouse <- gsub("\\( | \\) | \\-", "", variablestouse) # delete "()-"
variablestouse <- tolower(variablestouse)   # lower case

names(data.full)<-c("subject", "activity", variablestouse)
# names(data.full)

head(data.full)[1:10]

#export this file as cleaned_nonaggregated_data.txt
write.table(data.full, "cleaned_nonaggregated_data.txt")

# Task 5:
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject. 

install.packages("plyr")
library(plyr)

data.end <- ddply(data.full,.(subject, activity), colwise(mean)) # compute means over subjects and activities

write.table(data.end, "averaged_data.txt") # file export
