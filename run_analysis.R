#Step1 and Step4
x_train=read.table("./train/X_train.txt")
y_train=read.table("./train/y_train.txt")
subject_train=read.table("./train/subject_train.txt")
x_test=read.table("./test/X_test.txt")
y_test=read.table("./test/y_test.txt")
subject_test=read.table("./test/subject_test.txt")
feature=read.table("features.txt")
names=c(as.character(feature$V2),"Motion","SubjectID")
names=sub("-","",names)
x_train=cbind(x_train,y_train,subject_train)
x_test=cbind(x_test,y_test,subject_test)
colnames(x_train)=names
colnames(x_test)=names
data=rbind(x_test,x_train)

#Step2
Exindex=c(grep("mean[^F]|std",names),562,563)
data2=data[,Exindex]

#Step3
label=read.table("activity_labels.txt")
for (i in 1:6){data2$Motion=sub(i,label[i,2],data2$Motion)}

#Step5
id=rep(c(1:30),6)
motion=label[,2]
mo=rep(motion,6)
tidy=cbind(id,mo)
a=matrix(NA,180,66)
for (i in 1:30){
  for(j in 1:6){
    index=which(data2$Motion==label[j,2] & data2$SubjectID==i)
    temp=data2[index,c(1:66)]
    mean1=apply(temp,2,FUN=mean)
    k=(i-1)*6+j
    a[k,]=mean1
  }
}
tidy=cbind(tidy,a)
colnames(tidy)=c("ID","Motion",names(data2)[1:66])
head(tidy,n=12)
write.table(tidy,file="./tidy data.txt",sep="  ",col.names=TRUE)
