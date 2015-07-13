library(lubridate)
library(caret)
library(kernlab)
library(klaR)
library(rattle)
library(randomForest)
library(rpart)

train = read.csv('caterpillardata/competition_data/train_set.csv', stringsAsFactors = FALSE)
test = read.csv('caterpillardata/competition_data/test_set.csv', stringsAsFactors = FALSE)
summary(train)

# Convert to date.
train$quote_date <- as.Date(train$quote_date)
test$quote_date <- as.Date(test$quote_date)

# Extract different date components from date
train$year <- year(train$quote_date)
train$month <- month(train$quote_date)
train$dayofyear <- yday(train$quote_date)
train$dayofweek <- wday(train$quote_date)
train$day <- day(train$quote_date)

test$year <- year(test$quote_date)
test$month <- month(test$quote_date)
test$dayofyear <- yday(test$quote_date)
test$dayofweek <- wday(test$quote_date)
test$day <- day(test$quote_date)



# remove useless columns
test <- subset(test, select = -c(tube_assembly_id, quote_date, id, supplier))
train <- subset(train, select = -c(tube_assembly_id, quote_date, supplier))

# Convert to categorical

train$bracket_pricing <- factor(train$bracket_pricing)
test$bracket_pricing <- factor(test$bracket_pricing)

tc <- trainControl(method = "cv", number = 7, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

rf <- train(cost ~., data = train, method = "rf", trControl= tc)
svmr <- train(cost ~., data = train, method = "svmRadial", trControl= tc)
NN <- train(cost ~., data = train, method = "nnet", trControl= tc, verbose=FALSE)


svml <- train(cost ~., data = train, method = "svmLinear", trControl= tc)
bayesglm <- train(cost ~., data = train, method = "bayesglm", trControl= tc)
logitboost <- train(cost ~., data = train, method = "LogitBoost", trControl= tc)


glm.fit <- train(cost ~.,method="glm",data=train,trControl= tc)




# model <- c("Random Forest", "SVM (radial)","SVM (linear)","Neural Net", "Bayes GLM")
# Accuracy <- c(max(rf$results$Accuracy),
#               max(svmr$results$Accuracy),
#               max(svml$results$Accuracy),
#               max(NN$results$Accuracy),
#               max(bayesglm$results$Accuracy))
# 
# Kappa <- c(max(rf$results$Kappa),
#            max(svmr$results$Kappa),
#            max(svml$results$Kappa),
#            max(NN$results$Kappa),
#            max(bayesglm$results$Kappa))  
# 
# performance <- cbind(model,Accuracy,Kappa)
# 
# 
# knitr::kable(performance)
# 
# 
# rfPred <- predict(rf, test)
# svmrPred <- predict(svmr, test)
# 
# 
# prediction <- data.frame(cbind(rfPred, svmrPred))
# prediction$same <- with(prediction, rfPred == svmrPred)
# colnames(prediction) <- c("Random Forest", "SVM (radial)", "Same Prediction")


set.seed(825)
gbmFit1 <- train(cost ~ ., data = train,
                 method = "gbm",
                 trControl= tc,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

preds <- cbind(1:NROW(test), predict(gbmFit1, test))

colnames(preds) <- c('id', 'cost')

write.csv(preds, 'caterpillardata/gbmSubmission.csv', quote = FALSE, row.names = FALSE,na = "")

levels(train$bracket_pricing)


# 
# 
# gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
#                         n.trees = (1:30)*50,
#                         shrinkage = 0.1,
#                         n.minobsinnode = 10)
# 
# nrow(gbmGrid)
# 
# set.seed(825)
# gbmFit2 <- train(Class ~ ., data = training,
#                  method = "gbm",
#                  trControl = fitControl,
#                  verbose = FALSE,
#                  ## Now specify the exact models 
#                  ## to evaludate:
#                  tuneGrid = gbmGrid)
# gbmFit2










