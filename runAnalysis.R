




#first we read subject identifier for both datasets


subjectstrain <- read.delim("train/subject_train.txt",header = FALSE)
names(subjectstrain) <- c("subject")
subjectstext <- read.delim("test/subject_test.txt",header = FALSE)
names(subjectstext) <- c("subject")

# read activity table

activity <- read.table("activity_labels.txt")

# read both datasets for trainset and test set 


trainset <- read.table("train/X_train.txt")
testset <- read.table("test/X_test.txt")

# we assign the colnames for the features datasets, to both.
nomfeatures <- read.table("features.txt")
nomfeatures <- as.character(nomfeatures[,2])

colnames(trainset) <- nomfeatures
colnames(testset) <- nomfeatures

# we bind the subject identifier to the datasets as a new column. 

trainset <- cbind.data.frame(trainset,subjectstrain)
testset <- cbind.data.frame(testset,subjectstext)

#now we add the activity vector to both datasets

trainact <- read.table("train/y_train.txt")
testact <- read.table("test/y_test.txt")



trainset <- cbind.data.frame(trainset,trainact)
testset <- cbind.data.frame(testset,testact)

#now we merge both datasets into one. both train and test 

data1 <- rbind.data.frame(trainset,testset)

#change a variable name 


colnames(data1)[colnames(data1)=="V1"] <- "idactivity"

#by using merge, we add the activity as a description to the dataset. 

data2 <- merge(data1,activity)

#we correct the dataset names so we can subset it propoerly 

valid_column_names <- make.names(names=names(data2), unique=TRUE, allow_ = TRUE)
names(data2) <- valid_column_names

data2 <- select(data2, -V1)

colnames(data2)[colnames(data2)=="V2"] <- "labelactivity"

#subsetting of the dataset to those features that report either mean or std

colinteres <- grep("[Mm]ean|std",colnames(data2),value = TRUE)

#reorder the dataset so factors activitydespcription and subject are first columns

v1 <- colnames(data2)[562:564]

vsubset <- c(colinteres,v1)

alldata <- data2[vsubset]

cols <- data2[,c(562,564)]

alldata <- select(alldata, -c(subject,idactivity,labelactivity))

alldata <- cbind(cols,alldata)

# convert objective columns, activitylabel and subject to factor 

sapply(alldata[,1:2],class)

# subject labelactivity 
# "integer"      "factor" 

alldata$subject <- as.factor(alldata$subject)

#melting the dataset  

alldata_melted <- melt(alldata, id= c("subject","labelactivity")) 

#casting the melted data to obtain the mean. 

alldata_mean <- cast(alldata_melted, subject + labelactivity ~ variable, mean)

write.table(alldata_mean, "tidy.txt", row.names = FALSE, quote = FALSE)
  
  
  
  
  
  













