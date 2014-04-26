########read the data#########
test<-read.table("test/X_test.txt")
act_test<-read.table("test/y_test.txt")
train<-read.table("train/X_train.txt")
act_train<-read.table("train/y_train.txt")
subject_test<-read.table("test/subject_test.txt")
subject_train<-read.table("train/subject_train.txt")

########3.use decriptive activity names#########
act_test<-as.factor(act_test[,1])
act_train<-as.factor(act_train[,1])

label<-read.table("activity_labels.txt")
label<-as.character(label[,2])

#######1.merge training and test dataset###########
test<-data.frame(test,subject_test,act_test)
train<-data.frame(train,subject_train,act_train)

names<-read.table("features.txt")
names<-c(as.character(names$V2),"subject","activity")

names(test)<-names
names(train)<-names

data<-rbind(test,train)

#######4.label the merged dataset#############
levels(data$activity)<-label

#######2.extract measurements on the mean and standard deviation###############
means<-grep("[Mm]ean()",names)
stds<-grep("std()",names)
data<-data[,c(means,stds,562,563)]

#######5.create 
means_by_activity<-matrix(0,nrow=6,ncol=86,dimnames=list(levels(data$activity),names(data[1:86])))
for (i in 1:86) {
means_by_activity[,i]<-sapply(split(data[,i], data$activity), mean)
}

data<-data[order(data$subject),]
rownames<-paste("subject",unique(data$subject))
means_by_subject<-matrix(0,nrow=30,ncol=86,dimnames=list(rownames,names(data[1:86])))
for (i in 1:86) {
means_by_subject[,i]<-sapply(split(data[,i], data$subject), mean)
}

tidy_data<-list(means_by_subject=means_by_subject,means_by_activity=means_by_activity)

