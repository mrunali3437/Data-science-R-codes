#Boosting
#Data partition for model building and testing
data(iris)
install.packages("caret")
install.packages("C50")
#Library invoke
library(C50)
library(caret)
set.seed(102)

inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]
#Model Building
model<-C5.0(training$Species~.,data = training,trials=10) #Trials- Boosting parameter
#Generate the model summary
summary(model)

#Predict for test data set
pred<-predict.C5.0(model,testing[,-5])

a<-table(testing$Species,pred)
sum(diag(a))/sum(a)
