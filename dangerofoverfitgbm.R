library(gbm)

# impute.NA is a little function that fills in NAs with either means or medians
impute.NA <- function(x, fill="mean"){
  if (fill=="mean")
  {
    x.complete <- ifelse(is.na(x), mean(x, na.rm=TRUE), x)
  }
  
  if (fill=="median")
  {
    x.complete <- ifelse(is.na(x), median(x, na.rm=TRUE), x)
  }
  
  return(x.complete)
}

data <- read.table("Psychopath_Trainingset_v1.csv", header=T, sep=",")
testdata <- read.table("Psychopath_Testset_v1.csv", header=T, sep=",")

# Median impute all missing values
# Missing values are in columns 3-339
fulldata <- apply(data[,3:339], 2, FUN=impute.NA, fill="median")
data[,3:339] <- fulldata

fulltestdata <- apply(testdata[,3:339], 2, FUN=impute.NA, fill="median")
testdata[,3:339] <- fulltestdata

# Fit a generalized boosting model

# Create a formula that specifies that psychopathy is to be predicted using
# all other variables (columns 3-339) in the dataframe

gbm.psych.form <- as.formula(paste("psychopathy ~",
                                   paste(names(data)[c(3:339)], collapse=" + ")))

# Fit the model by supplying gbm with the formula from above.
# Including the train.fraction and cv.folds argument will perform
# cross-validation 

gbm.psych.bm.1 <- gbm(gbm.psych.form, n.trees=5000, data=data,
                      distribution="gaussian", interaction.depth=6,
                      train.fraction=.8, cv.folds=5)

# gbm.perf will return the optimal number of trees to use based on
# cross-validation. Although I grew 5,000 trees, cross-validation suggests that
# the optimal number of trees is about 4,332.

best.cv.iter <- gbm.perf(gbm.psych.bm.1, method="cv") # 4332

# Use the trained model to predict psychopathy from the test data. 

gbm.psych.1.preds <- predict(gbm.psych.bm.1, newdata=testdata, best.cv.iter)

# Package it in a dataframe and write it to a .csv file for uploading.

gbm.psych.1.bm.preds <- data.frame(cbind(myID=testdata$myID,
                                         psychopathy=gbm.psych.1.preds))

write.table(gbm.psych.1.bm.preds, "gbmbm1.csv", sep=",", row.names=FALSE)


par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  # here three plots are filled in with their respective titles
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  # this adds a line of text in the outer margin*
  mtext("Ozone and Weather in New York City", outer = TRUE)}
)



## Create plot on screen device
with(faithful, plot(eruptions, waiting))
## Add a main title
title(main = "Old Faithful Geyser data")




dev.copy(png, file = "geyserplot.png")

dev.off()
getwd()

library(lattice)
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x+ rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
## Plot with 2 panels with custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
  # call the default panel function for xyplot
  panel.xyplot(x, y, ...)
  # adds a horizontal line at the median
  panel.abline(h = median(y), lty = 2)
  # overlays a simple linear regression line
  panel.lmline(x, y, col = 2)
})


dev.off()
library(ggplot2)
qplot(displ, hwy, data = mpg, color = drv, shape = drv)


qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method="lm")
qplot(hwy, data = mpg, fill = drv)

qplot(displ, hwy, data = mpg, facets = . ~ drv)


qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)



#initiates ggplot
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
g + geom_point(alpha = 1/3)
+ facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4)
+ geom_smooth(method="lm", se=FALSE, col="steelblue")
+ theme_bw(base_family = "Avenir", base_size = 10)
+ labs(x = expression("log " * PM[2.5])
       + labs(y = "Nocturnal Symptoms”)
+ labs(title = "MAACS Cohort”)

       set.seed(1234)
       x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
       y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
       dataFrame <- data.frame(x=x,y=y)
       distxy <- dist(dataFrame)
       hClustering <- hclust(distxy)
       plot(hClustering)       
       
  
       # load data frame provided
       load("samsungData.rda")
       # table of 6 types of activities
       table(samsungData$activity)     
       
       # set up 1 x 2 panel plot
       par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
       # converts activity to a factor variable
       samsungData <- transform(samsungData, activity = factor(activity))
       # find only the subject 1 data
       sub1 <- subset(samsungData, subject == 1)
       # plot mean body acceleration in X direction
       plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1],
            main = "Mean Body Acceleration for X")
       # plot mean body acceleration in Y direction
       plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2],
            main = "Mean Body Acceleration for Y")
       # add legend
       legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)
       
       

       
       
       
       
       
       
       
      