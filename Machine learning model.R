library(dplyr)
library(tidyr)
setwd('C:/Users/GLC/Desktop/AIMS/REVIEW Course/BLOCK 4/Supervised and unsupervised Learning/0owR71Mml_Assignment_1_SUML')
#We start by loading our data set
mydata<-read.csv ('wine_dataset.csv', sep=',', stringsAsFactors=FALSE, header=TRUE)
attach(mydata)

#We remove the variable style and we create a new feature with just two class 'Nice' and 'worse'
mydata<-subset(mydata, select = -c(style))
mydata['qualty']<-ifelse(quality>5, 'Nice', 'Worse')
mydata<-subset(mydata, select = -c(quality))

attach(mydata)
#We visualize our data set
View(mydata)
str(mydata)

#We check the presence of missing values
anyNA(mydata)


prop.table(table(as.factor(mydata$qualty)))
#Boruta feature selection
library('Boruta')
set.seed(123)
boruta=Boruta(factor(quality)~.,data=mydata, doTrace=100)
print(boruta)
plot(boruta)


#We split our data in training and testing set with 70% for training

split<-sample(2, nrow(mydata), replace=T, prob=c(0.7, 0.3))

train<-mydata[split==1,]
test<-mydata[split==2,]

#we look at the dimension of our training and testing set

dim(train)
dim(test)

#We create our cross validation set
library(caret)
cross<-trainControl(method='cv', number=5)
#trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#We fit our support vector machine
set.seed(7)
modelSVM<-train(factor(qualty)~., data=train, method='svmRadial', trControl=cross)
modelSVM
predsvm<-predict(modelSVM, test)
confusionMatrix(predsvm, as.factor(test$qualty))


#We fit the random forest model
library(randomForest)
set.seed(7)
modelRF<-randomForest(factor(qualty)~., data=train, method='rf', trControl=cross)
modelRF
predsRF<-predict(modelRF, test)
confusionMatrix(predsRF, as.factor(test$qualty))


#We fit the KNN model
set.seed(7)
metric='Accuracy'
modelknn<-train(factor(qualty)~., data=train, method='knn', metric=metric, trControl=cross)
modelknn
predsknn<-predict(modelknn, test)
confusionMatrix(predsknn, as.factor(test$qualty))


#We fit Naive Bayes model
set.seed(10)
modelnb <- train(factor(qualty)~., data=train, method="naive_bayes", trControl=cross)
modelnb
prednb=predict(modelnb,newdata = test)
confusionMatrix(prednb,as.factor(test$qualty))


#We fit our Logistic regression model
set.seed(11)
modelLR <- train(factor(qualty)~., data=train, method="glm", family='binomial', trControl=cross)
modelLR
predLR=predict(modelLR,newdata = test)
confusionMatrix(predLR,as.factor(test$qualty))
