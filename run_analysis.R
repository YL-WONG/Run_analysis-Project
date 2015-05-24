#------------- Read all table files
train.x<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
test.x<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
train.y<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
test.y<-read.table("./data/UCI HAR Dataset/test/y_test.txt")
Subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
Subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")
activity_lables<-read.table("./data/UCI HAR Dataset/activity_labels.txt")
features<-read.table("./data/UCI HAR Dataset/features.txt")

#----------------Merges the training and the test sets to create one data set.
all.x<-rbind(train.x,test.x)

#----------------Extracts only the measurements on the mean and standard deviation for each measurement.
#---------------- i.e. Extract only columns that the column name contains "mean()"or "std()"

#-- Replace columns name by feature table's column 2 
colnames(all.x) <- c(as.character(features[,2]))

#-- Filter columns that column name contains "mean()"or "std()"
Mean<-grep("mean()",colnames(all.x),fixed=TRUE)

SD<-grep("std()",colnames(all.x),fixed=TRUE)

#-- Creat new data frame that only has above Filtered columns
MeanSD<-all.x[,c(Mean,SD)]

#----------------Uses descriptive activity names to name the activities in the data set.
#---------------- i.e. combine all y data (activity) with new data frame that only has above Filtered columns

all.y<-rbind(train.y,test.y)

all.activity<-cbind(all.y,MeanSD)

colnames(all.activity)[1] <- "Activity"

#----------------Appropriately labels the data set with descriptive activity names.
#----------------i.e. replace the activity number (y data - represent by number) by label stored in activity label table 
activity_lables[,2]<-as.character(activity_lables[,2])

for(i in 1:length(all.activity[,1])){
        all.activity[i,1]<-activity_lables[all.activity[i,1],2]
}

#----------------Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#----- Combine Subject with all.activity data frame
Subject_all<-rbind(Subject_train,Subject_test)

all<-cbind(Subject_all,all.activity)

colnames(all)[1] <- "Subject"

#----- calculate mean of column 3 group by Subject+Activity
Tidy <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )

#----- calculate mean of column 4-last column group by Subject+Activity
for(i in 4:ncol(all)){
        Tidy[,i] <- aggregate( all[,i] ~ Subject+Activity, data = all, FUN= "mean" )[,3]
}

colnames(Tidy)[3:ncol(Tidy)] <- colnames(MeanSD)

write.table(Tidy, file = "ResultData.txt")