#Data Load
data("iris")
#Install the required packages
install.packages("caret")

install.packages("C50")
#Library invoke
library(caret)
library(C50)
#To make the results consistent across the runs
attach(iris)

set.seed(7)
#Data Partition
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

#Model Building
model<-C5.0(Species~.,data = training) 
#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-5]) #type ="prob"
#Accuracy of the algorithm
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)
#Visualize the decision tree
plot(model)
##Bagging algorithm
## Bagging ----
# Using the ipred bagged decision trees
install.packages("ipred")
library(ipred)
set.seed(7)

mybag <- bagging(iris$Species ~ ., data = iris, nbagg = 2)
pred <- predict(mybag, iris)
table(pred, iris$Species)

# Bagging using Caret bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)

model_cv<-train(Species ~ ., data = iris, method = "treebag",trControl = ctrl)
pred <- predict(model_cv, testing)
table(pred, testing$Species)

#Boosting
#Data partition for model building and testing
data(iris)
library(C50)
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

