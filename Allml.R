library(nutshell)
data(spambase)
library(caret)
set.seed(100)
inTrain=createDataPartition(y=spambase$is_spam,p=.7,list=F)
spambase.training<-spambase[inTrain,]
spambase.validation<-spambase[-inTrain,]
library(MASS)
spam.qda<-qda(formula=is_spam~.,data=spambase.training)
summary(spam.qda)
table(actual=spambase.training$is_spam,predicted=predict(spam.qda,newdata=spambase.training)$class)
table(actual=spambase.validation$is_spam,predicted=predict(spam.qda,newdata=spambase.validation)$class)
library(mda)
spam.fda<-fda(formula=is_spam~.,data=spambase.training)
table(actual=spambase.validation$is_spam,predicted=predict(spam.fda,newdata=spambase.validation,type="class"))

spam.mda<-mda(formula=is_spam~.,data=spambase.training)
table(actual=spambase.validation$is_spam,predicted=predict(spam.mda,newdata=spambase.validation,type="class"))


spambase.knn<-knn(train=spambase.training,test=spambase.validation,cl = spambase.training$is_spam,k = 5)
summary(spambase.knn)
table(predicted=spambase.knn,acttual=spambase.validation$is_spam)

library(rpart)
spam.tree<-rpart(is_spam~.,data=spambase.training)
printcp(spam.tree)

library(maptree)
draw.tree(spam.tree,nodeinfo=T,cex=0.5,col=gray(0:8/8))
table(actual=spambase.validation$is_spam,prediction=predict(spam.tree,newdata=spambase.validation,type="class"))


library(adabag)
spam.bag<-bagging(formula=is_spam~.,data=spambase.training)
summary(spam.bag)

table(actual=spambase.training$is_spam,predicted=predict(spam.bag,newdata=spambase.training)$class)
table(actual=spambase.validation$is_spam,predicted=predict(spam.bag,newdata=spambase.validation)$class)


library(ada)
(spam.ada<-ada(formula=is_spam~.,data=spambase.training,loss="logistic"))
table(actual=spambase.training$is_spam,predicted=predict(spam.ada,newdata=spambase.training))
table(actual=spambase.validation$is_spam,predicted=predict(spam.ada,newdata=spambase.validation))


library(nnet)
spam.nnet<-nnet(is_spam~.,data=spambase.training,size=10,decay=0.1)
table(actual=spambase.training$is_spam,predicted=predict(spam.nnet,type="class"))
table(actual=spambase.validation$is_spam,predicted=predict(spam.nnet,newdata=spambase.validation,type="class"))

library(e1071)
spam.svm<-svm(is_spam~.,data=spambase.training)
spam.svm
table(actual=spambase.validation$is_spam,predicted=predict(spam.svm,newdata=spambase.validation,type="class"))

library(randomForest)
spam.rf<-randomForest(is_spam~.,data=spambase.training)
spam.rf
table(actual=spambase.validation$is_spam,predicted=predict(spam.rf,newdata=spambase.validation,type="class"))




