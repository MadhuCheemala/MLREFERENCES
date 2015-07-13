
require(knitr)
library(e1071)
set.seed(1)
#Create our own test data
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))


library(e1071)

dat <- data.frame(x=x, y=as.factor(y))
svm.fit <- svm(y ~., data=dat, kernel='linear', cost=10, scale=FALSE)
# Plot the SVC obtained
plot(svm.fit, dat)

summary(svm.fit)


svm.fit2 <- svm(y ~., data=dat, kernel = 'linear', cost=0.1, scale=FALSE)

plot(svm.fit2, dat)

summary(svm.fit2)



set.seed(1)
tune.out <- tune(svm, y ~., data=dat, kernel='linear',
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)


xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest [ ytest ==1 ,]= xtest [ ytest ==1 ,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

yhat <- predict(tune.out$best.model, testdat)
library(caret)
confusionMatrix(yhat, testdat$y)


# Generate some test data
set.seed (1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2

y <- c(rep(1,150),rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

plot(x, col=y)


train <- sample(200, 100)

svm.fit <- svm(y ~., data=dat[train,], kernel='radial', gamma=1, cost=1)

plot(svm.fit, dat[train,])



summary(svm.fit)



yhat <- predict(svm.fit, dat[-train,])
confusionMatrix(yhat, dat[-train,'y'])

svm.fit <- svm(y ~., dat[train,], kernel='radial', gamma=1, cost=1e5)

plot(svm.fit, dat[train,])

summary(svm.fit)
set.seed(1)
tune.out <- tune(svm, y ~., data=dat[train,], 
                 kernel='radial', 
                 ranges = list(cost=c(0.1,1,10,100,1000),
                               gamma=c(0.5, 1,2,3,4)))

summary(tune.out)



yhat <- predict(tune.out$best.model, dat[-train,])
confusionMatrix(yhat, dat[-train, 'y'])


library(ROCR)
# function to handle the different models
rocplot <- function(pred, truth, ...){
  predob =  prediction(pred, truth)
  perf = performance(predob, 'tpr', 'fpr')
  plot(perf, ...)
  
}



svm.opt <- svm(y ~., data=dat[train,], kernel='radial',
               gamma=2, cost=1, decision.values=T)

fitted <- attributes(predict(svm.opt, dat[train,], decision.values=T))$decision.values

rocplot(fitted, dat[train,'y'], main='Training Data')



ctr <- trainControl(method='cv',
                    number=10, 
                    classProbs=TRUE,
                    summaryFunction=twoClassSummary)

svm.c <- train(y ~., dat[train,],
               method='svmRadial',
               trControl=ctr,
               metric="pROC")




svm.c



plot(svm.c$finalModel)


yhat.c <- predict(svm.c, dat[-train,])
confusionMatrix(yhat.c, dat[-train,'y'])

attach(Auto)
library(MASS)


library(ISLR)


dim(Auto)
kable(head(Auto))
# Create a binary variable that takes on 1 for cars with gas mileage > median
Auto$y <- NA
Auto$y[Auto$mpg > median(Auto$mpg)] <- 1
Auto$y[Auto$mpg <= median(Auto$mpg)] <- 0
Auto$y <- as.factor(Auto$y)
length(Auto[is.na(Auto$y)]) # make sure there are no NA's

set.seed(123)
split <- createDataPartition(y=Auto$y, p=0.7, list=FALSE)
train <- Auto[split,]
test <- Auto[-split,]
# Remove mpg / name features
train <- train[-c(1,9)]
test <- test[-c(1,9)]

# 10 fold cross validation
ctr <- trainControl(method='repeatedcv',
                    number=10,
                    repeats=3)

# Recall as C increases, the margin tends to get wider
grid <- data.frame(C=seq(0.01,5,0.5))

svm.fit <- train(y ~., train,
                 method='svmLinear',
                 preProc=c('center','scale'),
                 trControl=ctr,
                 tuneGrid=grid)
svm.fit

# Training error rate
confusionMatrix(predict(svm.fit, train), train$y)


# Testing error rate
yhat <- predict(svm.fit, test)
confusionMatrix(yhat, test$y)

set.seed(123)
# Try a polynomial function
svm.fit <- train(y ~., train,
                 method='svmPoly',
                 trControl=ctr)

svm.fit


plot(svm.fit)

confusionMatrix(predict(svm.fit, train), train$y)
# Testing error rate
yhat <- predict(svm.fit, test)
confusionMatrix(yhat, test$y)













