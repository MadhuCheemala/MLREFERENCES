library(caret)
library(kernlab)
library(klaR)
library(rattle)
library(randomForest)
library(rpart)
set.seed(12345)

urlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


csvTrain <- "pml-training.csv"

if (file.exists(csvTrain)) {
  train <- read.csv(csvTrain, na.strings=c("NA","#DIV/0!",""))
} else { 
  download.file(urlTrain,csvTrain)
  train <- read.csv(csvTrain, na.strings=c("NA","#DIV/0!",""))
}                           

csvTest <-  "pml-testing.csv"

if (file.exists(csvTest)) {
  test <- read.csv(csvTest, na.strings=c("NA","#DIV/0!",""))
} else { 
  download.file(urlTest,csvTest)
  test <- read.csv(csvTest, na.strings=c("NA","#DIV/0!",""))
}   


all.equal(colnames(test)[1:length(colnames(test))-1], colnames(train)[1:length(colnames(train))-1])

summary(train)
summary(test)


nearzero <- nearZeroVar(train, saveMetrics = TRUE)
train <- train[, !nearzero$nzv]

toberem <- sapply(colnames(train), function(x) if(sum(is.na(train[, x])) > 0.50*nrow(train))    {return(TRUE)
}else{
  return(FALSE)
}
)
train <- train[, !toberem]

train <- train[, -(1:6)]


Hcorr <- caret::findCorrelation(cor(train[, -53]), cutoff=0.8)
names(train)[Hcorr]

names(train)


tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

rf <- train(classe ~ ., data = train, method = "rf", trControl= tc)
svmr <- train(classe ~ ., data = train, method = "svmRadial", trControl= tc)
NN <- train(classe ~ ., data = train, method = "nnet", trControl= tc, verbose=FALSE)


svml <- train(classe ~ ., data = train, method = "svmLinear", trControl= tc)
bayesglm <- train(classe ~ ., data = train, method = "bayesglm", trControl= tc)
logitboost <- train(classe ~ ., data = train, method = "LogitBoost", trControl= tc)


model <- c("Random Forest", "SVM (radial)","LogitBoost","SVM (linear)","Neural Net", "Bayes GLM")
Accuracy <- c(max(rf$results$Accuracy),
              max(svmr$results$Accuracy),
              max(logitboost$results$Accuracy),
              max(svml$results$Accuracy),
              max(NN$results$Accuracy),
              max(bayesglm$results$Accuracy))

Kappa <- c(max(rf$results$Kappa),
           max(svmr$results$Kappa),
           max(logitboost$results$Kappa),
           max(svml$results$Kappa),
           max(NN$results$Kappa),
           max(bayesglm$results$Kappa))  

performance <- cbind(model,Accuracy,Kappa)


knitr::kable(performance)


rfPred <- predict(rf, test)
svmrPred <- predict(svmr, test)


prediction <- data.frame(cbind(rfPred, svmrPred))
prediction$same <- with(prediction, rfPred == svmrPred)
colnames(prediction) <- c("Random Forest", "SVM (radial)", "Same Prediction")


knitr::kable(prediction)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(rfPred)
pml_write_files(svmrPred)



















