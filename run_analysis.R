## You should create one R script called run_analysis.R that does the following. 
#.....1. Merges the training and the test sets to create one data set.
#.....2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#.....3. Uses descriptive activity names to name the activities in the data set
#.....4. Appropriately labels the data set with descriptive variable names. 
#.....5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Read the raw data

rawTrainX<-read.table("./data/train/X_train.txt")
rawTestX<-read.table("./data/test/X_test.txt")
rawTrainY<-read.table("./data/train/y_train.txt")
rawTestY<-read.table("./data/test/y_test.txt")
subjectTrain<-read.table("./data/train/subject_train.txt")
subjectTest<-read.table("./data/test/subject_test.txt")
activityLables<-read.table("./data/activity_labels.txt")
features<-read.table("./data/features.txt")

##Merges the training and the test sets to create one data set.

rawDataX<-rbind(rawTrainX,rawTestX)

##Extracts only the measurements on the mean and standard deviation for each measurement.

colnames(rawDataX) <- c(as.character(features[,2]))

dataMean<-grep("mean()",colnames(rawDataX),fixed=TRUE)

dataStandardDev<-grep("std()",colnames(rawDataX),fixed=TRUE)

meanSD<-rawDataX[,c(dataMean,dataStandardDev)]

##Uses descriptive activity names to name the activities in the data set

rawDataXY<-rbind(rawTrainY,rawTestY)

activityData<-cbind(rawDataXY,meanSD)

colnames(activityData)[1] <- "Activity"

##Appropriately labels the data set with descriptive activity names. 

activityLables[,2]<-as.character(activityLables[,2])

for(i in 1:length(activityData[,1])){
  activityData[i,1]<-activityLables[activityData[i,1],2]
}

# ##Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectAll<-rbind(subjectTrain,subjectTest)

finalData<-cbind(subjectAll,activityData)

colnames(finalData)[1] <- "Subject"

tidyData <- aggregate( finalData[,3] ~ Subject+Activity, data = finalData, FUN= "mean" )

for(i in 4:ncol(finalData)){
  tidyData[,i] <- aggregate( finalData[,i] ~ Subject+Activity, data = finalData, FUN= "mean" )[,3]
}

colnames(tidyData)[3:ncol(tidyData)] <- colnames(meanSD)

#write the tidy dataset deliverable to a file

write.table(tidyData, file = "tidyData.txt")